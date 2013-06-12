/*
 * $Id: PDFSigner.java 264 2012-06-12 08:19:37Z ahto.truu $
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

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.MalformedURLException;
import java.util.HashMap;

import com.guardtime.format.FormatException;
import com.guardtime.transport.SimpleHttpStamper;
import com.guardtime.tsp.GTDataHash;
import com.guardtime.tsp.GTException;
import com.guardtime.tsp.GTHashAlgorithm;
import com.guardtime.tsp.GTTimestamp;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Image;
import com.lowagie.text.Rectangle;
import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfImportedPage;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.PdfSignatureAppearance;
import com.lowagie.text.pdf.PdfStamper;
import com.lowagie.text.pdf.PdfString;
import com.lowagie.text.pdf.collection.PdfCollection;

/**
 * Class for signing and modifying PDF documents.
 * <p>
 * Current implementation relies on the iText library which was dual-licensed
 * under the LGPL and MPL up to versions 2.1.x, but then changed to AGPL and
 * jumped the version number to 5 in the late 2009. Please refer to the
 * documentation of the iText library for full legal details.
 */
public class PDFSigner {

	/**
	 * The field type for a document-level time-stamp, as defined in the PAdES
	 * specification (ETSI TS 102 778, Part 4, Section A.2).
	 */
	private static final String TS_FT = "DocTimeStamp";

	/**
	 * The name of the preferred handler for GuardTime time-stamps, as defined
	 * in the PDF specification (ISO 32000-1:2008, Section 12.8), and registered
	 * in the PDF Name Registry maintained by Adobe.
	 */
	private static final String TS_FILTER = "GTTS.TimeStamp";

	/**
	 * The data type for a document-level time-stamp, as defined in the PAdES
	 * specification (ETSI TS 102 778, Part 4, Section A.2).
	 */
	private static final String TS_SUBFILTER = "ETSI.RFC 3161";

	/**
	 * We slightly abuse the Name field of the time-stamp structure in the PDF.
	 * The value of this field will only be shown in PDF viewers where the
	 * verification plug-in has not been installed. Since this seems to be the
	 * only field displayed in recent versions of Adobe Reader without a
	 * verification plug-in, we use it to direct the user to install the
	 * plug-in.
	 */
	private static final String TS_NAME = "[Get the verification plug-in from http://download.guardtime.com/]";

	/**
	 * Internal formatting constant: the maximal allowed size of the time-stamp
	 * object; due to the way time-stamps are embedded in PDF documents, a fixed
	 * amount of space must be set aside before creating the time-stamp. Thus,
	 * the value here should be large enough to accommodate any time-stamp we
	 * ever create.
	 */
	private static final int TS_MAX_SIZE = 8192;

	/**
	 * IText {@code PdfStamper} used for PDF modifications.
	 */
	private PdfStamper stamper;
		
	/**
	 * State of the PDFSigner.
	 * <p>
	 * Underlying iText {@code PdfStamper} cannot be used after signature is
	 * closed, thus we need to keep its state and throw exceptions if PDFSigner
	 * is used after stamping.
	 */
	private boolean isOpen;

	/**
	 * Creates a new PDF Signer.
	 * 
	 * @param file
	 *            the file to be updated in place.
	 * 
	 * @throws IOException
	 *             when there are errors reading or writing the file or parsing
	 *             the document.
	 * @throws DocumentException
	 *             when there are errors creating the signature structures in
	 *             the document.
	 */
	public PDFSigner(File file)
	throws IOException, DocumentException {
		this(file, file);
	}

	/**
	 * Creates a new PDF Signer.
	 * <p>
	 * The files {@code in} and {@code out} may be the same file; {@code out}
	 * will not be written to before {@code in} has been read completely.
	 * 
	 * @param in
	 *            the file to read the original document from.
	 * @param out
	 *            the file to write the signed result to.
	 * 
	 * @throws IOException
	 *             when there are errors reading or writing the files or parsing
	 *             the document.
	 * @throws DocumentException
	 *             when there are errors creating the signature structures in
	 *             the document.
	 */
	public PDFSigner(File in, File out)
	throws IOException, DocumentException {
		this(new FileInputStream(in), new LazyFileOutputStream(out));
	}

	/**
	 * Creates a new PDF Signer.
	 * 
	 * @param in
	 *            the stream to read the original document from.
	 * @param out
	 *            the stream to write the signed result to.
	 * 
	 * @throws IOException
	 *             when there are errors reading or writing the streams or
	 *             parsing the document.
	 * @throws DocumentException
	 *             when there are errors creating the signature structures in
	 *             the document.
	 */
	public PDFSigner(InputStream in, OutputStream out)
	throws IOException, DocumentException {
		File tmpFile = null;
		try {
			tmpFile = File.createTempFile("gtPDF", ".pdf");
		} catch (Exception e) {
			// temporary file will not be used
		}

		PdfReader reader = new PdfReader(in);
		stamper = PdfStamper.createSignature(reader, out, '\0', tmpFile, true);
		isOpen = true;
	}

	/**
	 * Adds a document level attachment to the original PDF.
	 * 
	 * @param description
	 *            file description.
	 * @param file
	 *            attachment.
	 * 
	 * @throws IOException
	 *             when there are errors reading the file or updating the PDF
	 *             document.
	 */
	public void addAttachment(String description, File file)
	throws IOException {
		checkOpen();
		stamper.addFileAttachment(description, null, file.getAbsolutePath(), file.getName());
	}

	/**
	 * Adds a document level attachment to the original PDF.
	 * 
	 * @param description
	 *            file description.
	 * @param filename
	 *            file name displayed in Reader.
	 * @param fileData
	 *            file data.
	 * 
	 * @throws IOException
	 *             when there are errors updating the PDF document.
	 */
	public void addAttachment(String description, String filename, byte[] fileData)
	throws IOException {
		checkOpen();
		stamper.addFileAttachment(description, fileData, null, filename);
	}

	/**
	 * Adds default Collection element to the PDF thus turning the PDF into a
	 * PDF Collection.
	 * 
	 * @throws IOException
	 *             when there are errors updating the PDF document.
	 */
	public void makeCollection()
	throws IOException {
		checkOpen();		
		PdfCollection collection = new PdfCollection(PdfCollection.TILE);
		stamper.makePackage(collection);
	}

	/**
	 * Checks {@code PDFSigner} state and throws {@code IllegalStateException}
	 * if it is not open.
	 */
	private void checkOpen() {
		if (!isOpen) {
			throw new IllegalStateException("PDFSigner is closed");
		}
	}

	/**
	 * Adds a GuardTime signature to the document and writes the result. After
	 * signing this object is considered closed and any successive method calls
	 * will throw {@code IllegalStateException}.
	 * 
	 * @param svc
	 *            the configuration for accessing GuardTime service.
	 * @param seal
	 *            the visual "seal" image to add to the document; an invisible
	 *            time-stamp will be added if this is {@code null}.
	 * @param name
	 *            the "signer name" to add to the document metadata; the default
	 *            from TS_NAME will be used if this is {@code null}.
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
	public void sign(ServiceConfiguration svc, SealConfiguration seal, String name)
	throws DocumentException, MalformedURLException, IOException, GTException {
		checkOpen();
		
		PdfSignatureAppearance sap = stamper.getSignatureAppearance();

		// if a "seal" image is given, add it to the document
		if (seal != null) {

			// convert the image data to a PDF image object
			Image img = null;
			byte[] b = seal.getImage();
			if (b.length > 3 && b[0] == '%' && b[1] == 'P' && b[2] == 'D' && b[3] == 'F') {
				// special handling for importing another PDF document as a seal image
				PdfReader r = new PdfReader(b);
				PdfImportedPage p = stamper.getImportedPage(r, 1);
				img = Image.getInstance(p);
			} else {
				img = Image.getInstance(b);
			}

			// process the requested page number
			int page = seal.getPage();
			int pages = stamper.getReader().getNumberOfPages();
			if (page == 0 || Math.abs(page) > pages) {
				throw new DocumentException("Page number " + page + " out of range; document has " + pages + " pages");
			}
			if (page < 0) {
				page += pages + 1;
			}

			// extract the printable area of the page
			int rot = stamper.getReader().getPageRotation(page);
			if (rot < 0 || rot >= 360 || rot % 90 != 0) {
				throw new DocumentException("Unsupported page rotation " + rot + " specified in document");
			}
			Rectangle pageSize = stamper.getReader().getCropBox(page);
			while (pageSize.getRotation() != rot) {
				pageSize = pageSize.rotate();
			}

			// process the requested image size
			float w = seal.getWidth();
			float h = seal.getHeight();
			if (!(w >= 0 && h >= 0)) {
				throw new DocumentException("Negative image dimensions " + w + " by " + h);
			}
			float imgW = img.getPlainWidth();
			float imgH = img.getPlainHeight();
			float pageW = pageSize.getWidth();
			float pageH = pageSize.getHeight();
			if (w > 0 && h > 0) {
				imgW = w;
				imgH = h;
			} else if (w > 0 && imgW > 0) {
				float r = w / imgW;
				imgW *= r;
				imgH *= r;
			} else if (h > 0 && imgH > 0) {
				float r = h / imgH;
				imgW *= r;
				imgH *= r;
			} else {
				// nothing here, the image is used in original size
			}
			if (imgW > pageW || imgH > pageH) {
				throw new DocumentException("Scaled image dimensions " + imgW + " by " + imgH
						+ " exceed the page dimensions " + pageW + " by " + pageH);
			}

			// position the image horizontally
			float x = seal.getX();
			float pageL = pageSize.getLeft();
			float pageR = pageSize.getRight();
			float imgL, imgR;
			if (x == 0) {
				// centered
				imgL = (pageL + pageR) / 2 - imgW / 2;
				imgR = (pageL + pageR) / 2 + imgW / 2;
			} else if (x > 0) {
				// left-aligned
				imgL = pageL + x;
				imgR = imgL + imgW;
			} else if (x < 0) {
				// right-aligned
				imgR = pageR + x;
				imgL = imgR - imgW;
			} else {
				throw new DocumentException("Requested image position " + x + " is not a number");
			}
			if (imgL < pageL || imgR > pageR) {
				throw new DocumentException("Aligned image position " + imgL + " to " + imgR + " outside page width "
						+ pageL + " to " + pageR);
			}

			// position the image vertically
			float y = seal.getY();
			float pageB = pageSize.getBottom();
			float pageT = pageSize.getTop();
			float imgT, imgB;
			if (y == 0) {
				// centered
				imgT = (pageB + pageT) / 2 + imgH / 2;
				imgB = (pageB + pageT) / 2 - imgH / 2;
			} else if (y > 0) {
				// top-aligned
				imgT = pageT - y;
				imgB = imgT - imgH;
			} else if (y < 0) {
				// bottom-aligned
				imgB = pageB - y;
				imgT = imgB + imgH;
			} else {
				throw new DocumentException("Requested image position " + y + " is not a number");
			}
			if (imgB < pageB || imgT > pageT) {
				throw new DocumentException("Aligned image position " + imgB + " to " + imgT + " outside page height "
						+ pageB + " to " + pageT);
			}

			// add the image to the page
			Rectangle pos = new Rectangle(imgL, imgB, imgR, imgT);
			sap.setVisibleSignature(pos, page, null);
			sap.setImage(img);
			sap.setLayer2Text("");
			sap.setLayer4Text("");
			sap.setAcro6Layers(true);
		}

		// set the cryptographic attributes of the time-stamp object
		PdfDictionary dic1 = new PdfDictionary();
		dic1.put(PdfName.FT, new PdfName(TS_FT));
		dic1.put(PdfName.FILTER, new PdfName(TS_FILTER));
		dic1.put(PdfName.SUBFILTER, new PdfName(TS_SUBFILTER));
		dic1.put(PdfName.NAME, new PdfString(name == null ? TS_NAME : name));
		sap.setCryptoDictionary(dic1);
		HashMap<PdfName, Object> exc = new HashMap<PdfName, Object>();
		exc.put(PdfName.CONTENTS, new Integer(2 * TS_MAX_SIZE + 2));
		sap.preClose(exc);

		// create the time-stamp
		GTDataHash dh = new GTDataHash(GTHashAlgorithm.DEFAULT);
		dh.update(sap.getRangeStream());
		GTTimestamp ts = SimpleHttpStamper.create(dh, svc.getStamper());
		byte[] tsb = ts.getEncoded();

		// insert the time-stamp into the buffer
		PdfDictionary dic2 = new PdfDictionary();
		dic2.put(PdfName.CONTENTS, new PdfString(tsb).setHexWriting(true));
		sap.close(dic2);
				
		isOpen = false;
	}

}
