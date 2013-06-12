/*
 * $Id: SignedPDF.java 234 2011-12-19 22:37:50Z ahto.truu $
 *
 * Copyright 2008-2011 GuardTime AS
 *
 * This file is part of the GuardTime PDF Toolkit, an addendum
 * to the GuardTime Client SDK for Java.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.guardtime.pdf;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import com.guardtime.format.FormatException;
import com.guardtime.transport.SimpleHttpStamper;
import com.guardtime.tsp.GTDataHash;
import com.guardtime.tsp.GTException;
import com.guardtime.tsp.GTTimestamp;
import com.guardtime.tsp.VerificationResult;
import com.guardtime.util.Util;
import com.lowagie.text.DocumentException;
import com.lowagie.text.pdf.AcroFields;
import com.lowagie.text.pdf.ByteBuffer;
import com.lowagie.text.pdf.PdfArray;
import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfNumber;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.PdfString;

/**
 * Encapsulates a signed PDF document and provides operations to manipulate
 * existing GuardTime time-stamps embedded in the document.
 * <p>
 * Current implementation relies on the iText library which was dual-licensed
 * under the LGPL and MPL up to versions 2.1.x, but then changed to AGPL and
 * jumped the version number to 5 in the late 2009. Please refer to the
 * documentation of the iText library for full legal details.
 */
public class SignedPDF {

	/**
	 * The name of the preferred handler for GuardTime time-stamps, as defined
	 * in the PDF specification (ISO 32000-1:2008, Section 12.8), and registered
	 * in the PDF Name Registry maintained by Adobe.
	 */
	private static final String TS_FILTER = "GTTS.TimeStamp";
		
	/**
	 * Current contents of the document.
	 */
	private byte[] doc;

	/**
	 * A PDF parser object initialized to the current contents.
	 */
	private PdfReader rdr;

	/**
	 * List of signature objects (both GuardTime and others) found in the
	 * document. The list is ordered from newest to oldest, as defined by the
	 * ordering of the document revisions they sign.
	 * 
	 * @see #getTimestamps()
	 * @see #getTimestamp()
	 */
	private List<SignatureInfo> sig;
	
	/**
	 * Creates a new document from the given file.
	 * 
	 * @param file
	 *            the file to load the document from.
	 * 
	 * @throws IOException
	 *             when there are errors reading from the file or parsing its
	 *             contents as a PDF document.
	 * @throws FormatException
	 *             when there are errors parsing the signatures embedded in the
	 *             document.
	 * 
	 * @see #SignedPDF(InputStream)
	 */
	public SignedPDF(File file)
	throws IOException, FormatException {
		this(new FileInputStream(file));
	}

	/**
	 * Creates a new document from the given stream.
	 * 
	 * @param is
	 *            the stream to load the document from.
	 * 
	 * @throws IOException
	 *             when there are errors reading from the stream or parsing its
	 *             contents as a PDF document.
	 * @throws FormatException
	 *             when there are errors parsing the signatures embedded in the
	 *             document.
	 * 
	 * @see #SignedPDF(File)
	 */
	public SignedPDF(InputStream is)
	throws IOException, FormatException {
		setDoc(Util.readAll(is));			
	}
		
	/**
	 * Verifies the GuardTime time-stamp in the document.
	 * 
	 * @param si
	 *            the time-stamp to verify.
	 * @param svc
	 *            the configuration for accessing GuardTime service.
	 * @return the verification result as a {@code GTVerificationResult} object;
	 *         use the {@code isValid()} method for just a yes/no answer, or
	 *         {@code getError()} and {@code getStatus()} methods for more
	 *         details.
	 * 
	 * @throws IOException
	 *             when there are errors accessing the GuardTime service.
	 * @throws GTException
	 *             when the electronic archive of control publications
	 *             (Integrity Codes) is invalid.
	 */
	public VerificationResult verifyTimestamp(SignatureInfo si, ServiceConfiguration svc)
	throws IOException, GTException {
		GTTimestamp ts = si.getTimestamp();
		GTDataHash dh = new GTDataHash(ts.getHashAlgorithm());
		dh.update(doc, si.range[0], si.range[1]);
		dh.update(doc, si.range[2], si.range[3]);
		return SimpleHttpStamper.verify(ts, dh, svc.getExtender(), null, svc.getPublications()).getGtResult();
	}
	
	/**
	 * Returns all GuardTime time-stamps from the document.
	 * 
	 * @return all time-stamps, ordered from newest to oldest.
	 * 
	 * @see #getTimestamp()
	 */
	public List<SignatureInfo> getTimestamps() {
		List<SignatureInfo> res = new ArrayList<SignatureInfo>();
		for (Iterator<SignatureInfo> it = sig.iterator(); it.hasNext(); ) {
			SignatureInfo si = it.next();
			if (si.getTimestamp() != null) {
				res.add(si);
			}
		}
		return res;
	}

	/**
	 * Returns the most recent GuardTime time-stamp from the document.
	 * 
	 * @return the time-stamp, or {@code null} if there is none.
	 * 
	 * @see #getTimestamps()
	 */
	public SignatureInfo getTimestamp() {
		for (Iterator<SignatureInfo> it = sig.iterator(); it.hasNext(); ) {
			SignatureInfo si = it.next();
			if (si.getTimestamp() != null) {
				return si;
			}
		}
		return null;
	}
	
	/**
	 * Extends the GuardTime time-stamp in the document.
	 * <p>
	 * Note that if there are several signatures in a PDF document, each
	 * subsequent one normally signs over all the previous ones and therefore
	 * only the very last signature object in the document can be updated
	 * without invalidating others.
	 * <p>
	 * This method checks for the existence of other signatures and a
	 * {@code DocumentException} is thrown if extending the GuardTime time-stamp
	 * would invalidate other signatures.
	 * <p>
	 * For the above reasons, only the most recent GuardTime time-stamp can be
	 * extended in case there are several in the document. This is also why this
	 * method does not have a parameter to indicate which time-stamp is to be
	 * extended.
	 * 
	 * @param svc
	 *            the configuration for accessing GuardTime service.
	 * 
	 * @throws IOException
	 *             when there are errors parsing the document.
	 * @throws DocumentException
	 *             when there are errors creating the signature structures in
	 *             the document.
	 * @throws GTException
	 *             when there are errors accessing the GuardTime service.
	 * @throws FormatException
	 *             when there are errors parsing the signatures embedded in the
	 *             document.
	 */
	public void extendTimestamp(ServiceConfiguration svc)
	throws IOException, DocumentException, GTException, FormatException {

		// retrieve the most recent time-stamp
		SignatureInfo si = getTimestamp();
		if (si == null) {
			throw new DocumentException("No GuardTime signatures in the document");
		}
		if (!si.isLast()) {
			throw new DocumentException("The GuardTime signature has been signed over and can't be updated");
		}

		// extend the time-stamp
		GTTimestamp ts = SimpleHttpStamper.extend(si.getTimestamp(), svc.getExtender());
		byte[] tsb = ts.getEncoded();
		
		// encode the time-stamp
		ByteBuffer buf = new ByteBuffer();
		buf.append('<');
		for (int i = 0; i < tsb.length; ++i) {
			buf.appendHex(tsb[i]);
		}
		buf.append('>');
		int len = si.getSignatureLength();
		if (buf.size() > len) {
			throw new DocumentException("The extended signature is too long and can't be inserted into the document");
		}
		while (buf.size() < len) {
			buf.append(' ');
		}

		// replace the time-stamp and reload the document
		System.arraycopy(buf.getBuffer(), 0, doc, si.getSignatureStart(), si.getSignatureLength());
		setDoc(doc);

	}
	
	/**
	 * Sets the contents of the document and resets all other structures.
	 * 
	 * @param doc
	 *            document contents.
	 * 
	 * @throws IOException
	 *             when there are errors parsing the contents as a PDF document.
	 * @throws FormatException
	 *             when there are errors parsing the signatures embedded in the
	 *             document.
	 */
	private void setDoc(byte[] doc)
	throws IOException, FormatException {

		// this.doc
		this.doc = doc;

		// this.rdr
		rdr = new PdfReader(new ByteArrayInputStream(doc));

		// this.sig
		sig = new ArrayList<SignatureInfo>();
		AcroFields flds = rdr.getAcroFields();
		for (Iterator it = flds.getSignatureNames().iterator(); it.hasNext(); ) {
			String name = (String) it.next();
			PdfDictionary dict = flds.getSignatureDictionary(name);
			boolean whole = flds.signatureCoversWholeDocument(name);
			sig.add(new SignatureInfo(name, dict, whole));
		}
		if (!sig.isEmpty()) {
			Collections.sort(sig);
			sig.get(0).markLast();
		}

	}

	/**
	 * Writes the current document to a file.
	 * 
	 * @param file
	 *            the file to write the document to.
	 * 
	 * @throws IOException
	 *             when there are errors writing to the file.
	 * 
	 * @see #write(OutputStream)
	 */
	public void write(File file)
	throws IOException {
		OutputStream os = new FileOutputStream(file);
		write(os);
		os.close();
	}

	/**
	 * Writes the current document to a stream.
	 * 
	 * @param os
	 *            the stream to write the document to.
	 * 
	 * @throws IOException
	 *             when there are errors writing to the stream.
	 * 
	 * @see #write(File)
	 */
	public void write(OutputStream os)
	throws IOException {
		os.write(doc);
	}
	
	
	/**
	 * A helper class to carry information about a PDF signature object
	 * extracted from the document.
	 * <p>
	 * Note: this class has a natural ordering that is inconsistent with equals.
	 */
	public class SignatureInfo
	implements Comparable<SignatureInfo> {

		/**
		 * The the index of the entry in the RANGE array that shows the start of
		 * the first signed section.
		 */
		private static final int TS_RANGE_START_1 = 0;

		/**
		 * The the index of the entry in the RANGE array that shows the length of
		 * the first signed section.
		 */
		private static final int TS_RANGE_LENGTH_1 = 1;

		/**
		 * The the index of the entry in the RANGE array that shows the start of
		 * the second signed section.
		 */
		private static final int TS_RANGE_START_2 = 2;

		/**
		 * The the index of the entry in the RANGE array that shows the length of
		 * the second signed section.
		 */
		private static final int TS_RANGE_LENGTH_2 = 3;

		/**
		 * The required number of entries in the RANGE array of a PDF signature.
		 */
		private static final int TS_RANGE_SIZE = 4;

		/**
		 * Name of the PDF field the signature was extracted from.
		 * 
		 * @see #getName()
		 */
		private String name;

		/**
		 * The GuardTime time-stamp object, if the signature is one.
		 * 
		 * @see #getTimestamp()
		 */
		private GTTimestamp stamp;

		/**
		 * The byte range covered by the signature.
		 * 
		 * @see #getSignatureStart()
		 * @see #getSignatureLength()
		 */
		private int[] range = new int[TS_RANGE_SIZE];

		/**
		 * Whether the signature is over the whole document.
		 * 
		 * @see #isWhole()
		 */
		private boolean whole;

		/**
		 * Whether this is the last signature in the document.
		 * 
		 * @see #markLast()
		 * @see #isLast()
		 */
		private boolean last;

		/**
		 * Constructs a new signature info object.
		 * 
		 * @param name
		 *            name of the PDF field the signature was extracted from.
		 * @param dict
		 *            the dictionary object containing the signature data.
		 * @param whole
		 *            whether the signature is over the whole document.
		 * 
		 * @throws FormatException
		 *             when the {@code PdfDictionary} does not represent a
		 *             correctly formatted signature.
		 */
		protected SignatureInfo(String name, PdfDictionary dict, boolean whole)
		throws FormatException {

			// this.name
			this.name = name;

			// this.stamp
			PdfName filter = dict.getAsName(PdfName.FILTER);
			if (filter == null) {
				throw new FormatException("Invalid signature format in field " + name + ": no FILTER");
			}
			if (filter.toString().equals("/" + TS_FILTER)) {
				// a GuardTime time-stamp, we need the contents
				PdfString stamp = dict.getAsString(PdfName.CONTENTS);
				if (stamp == null) {
					throw new FormatException("Invalid signature format in field " + name + ": no CONTENTS");
				}
				try {
					this.stamp = GTTimestamp.getInstance(stamp.getBytes());
				} catch (GTException x) {
					throw new FormatException("Invalid signature format in field " + name + ": invalid CONTENTS", x);
				}
			} else {
				// a non-GuardTime signature, we don't need the contents
				this.stamp = null;
			}

			// this.range
			PdfArray range = dict.getAsArray(PdfName.BYTERANGE);
			if (range == null) {
				throw new FormatException("Invalid signature format in field " + name + ": no BYTERANGE");
			}
			if (range.size() != this.range.length) {
				throw new FormatException("Invalid signature format in field " + name + ": invalid BYTERANGE size");
			}
			for (int i = 0; i < this.range.length; ++i) {
				PdfNumber n = range.getAsNumber(i);
				if (n == null) {
					throw new FormatException("Invalid signature format in field " + name + ": invalid BYTERANGE");
				}
				this.range[i] = n.intValue();
			}

			// this.whole
			this.whole = whole;

			// this.last will be set after the list of signatures is sorted
			this.last = false;

		}

		/**
		 * Returns the name of the PDF field the signature was extracted from.
		 * 
		 * @return the name of the PDF field the signature was extracted from.
		 */
		public String getName() {
			return name;
		}

		/**
		 * Returns the GuardTime time-stamp object, if the signature is one.
		 * <p>
		 * If the signature is in fact a GuardTime time-stamp, this method
		 * returns the time-stamp object. Otherwise, the method returns
		 * {@code null}.
		 * 
		 * @return the time-stamp as {@code GTTimestamp} object, or {@code null}.
		 */
		public GTTimestamp getTimestamp() {
			return stamp;
		}

		/**
		 * Returns the starting position of the signature value within the
		 * document.
		 * 
		 * @return the starting position of the signature value within the
		 *         document.
		 * 
		 * @see #getSignatureLength()
		 */
		protected int getSignatureStart() {
			return range[TS_RANGE_START_1] + range[TS_RANGE_LENGTH_1];
		}

		/**
		 * Returns the length of the signature value within the document.
		 * 
		 * @return the length of the signature value within the document.
		 * 
		 * @see #getSignatureStart()
		 */
		protected int getSignatureLength() {
			return range[TS_RANGE_START_2] - (range[TS_RANGE_START_1] + range[TS_RANGE_LENGTH_1]);
		}

		/**
		 * Returns whether the signature is over the whole document.
		 * <p>
		 * It is technically possible for any signature to cover only part of
		 * the document, but if all signers follow the PDFD specification, then
		 * a signature covering only part of the document should mean that there
		 * are revisions made to the document since it was signed.
		 * 
		 * @return whether the signature covers the while document.
		 */
		public boolean isWhole() {
			return whole;
		}

		/**
		 * Marks this as the last signature (whether GuardTime or other) in the
		 * document.
		 * 
		 * @see #isLast()
		 */
		protected void markLast() {
			last = true;
		}

		/**
		 * Returns whether this is the last signature (either GuardTime or
		 * other) in the document.
		 * 
		 * @return whether this is the last signature in the document.
		 * 
		 * @see #markLast()
		 */
		public boolean isLast() {
			return last;
		}

		/**
		 * This is for ordering the signatures in reverse order of the revisions
		 * of the document.
		 */
		public int compareTo(SignatureInfo that) {
			int thisEnd = this.range[TS_RANGE_START_2] + this.range[TS_RANGE_LENGTH_2];
			int thatEnd = that.range[TS_RANGE_START_2] + that.range[TS_RANGE_LENGTH_2];
			return thatEnd - thisEnd;
		}

	} // class SignatureInfo

}
