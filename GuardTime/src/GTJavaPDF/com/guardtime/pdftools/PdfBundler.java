/*
 * $Id: PdfBundler.java 283 2013-03-28 16:42:48Z ahto.truu $
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
package com.guardtime.pdftools;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.StringReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.Iterator;

import com.guardtime.pdf.PDFSigner;
import com.guardtime.pdf.SealConfiguration;
import com.guardtime.pdf.ServiceConfiguration;
import com.lowagie.text.Document;
import com.lowagie.text.Image;
import com.lowagie.text.ListItem;
import com.lowagie.text.PageSize;
import com.lowagie.text.html.simpleparser.HTMLWorker;
import com.lowagie.text.pdf.PdfImportedPage;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.PdfWriter;

/**
 * A sample utility to bundle sets of files into signed PDFs.
 */
public abstract class PdfBundler {

	/**
	 * The program title.
	 */
	private static final String TITLE = "GuardTime PDF Bundling Tool v.0.3.2-scrive";

	/**
	 * Default output file name.
	 */
	private static final String DEFAULT_OUTPUT = "bundle.pdf";

	/**
	 * Usage message for the CLI.
	 */
	private static final String USAGE = TITLE + "\n" +
			"Usage: java -jar PdfBundler.jar [options] file ...\n" +
			"\tto bundle all named files into a PDF and sign the result\n" +
			"Options can be:\n" +
			"\t-l logo - name of the file to load the cover page logo from\n" +
			"\t     PDF, WMF, PNG, GIF, TIFF, JPEG formats supported\n" +
			"\t-t title - title to be shown on the cover page under the logo\n" +
			"\t     basic HTML formatting supported; should be enclosed in quotes\n" +
			"\t-n name - the signer name to be added to the document metadata\n" +
			"\t     only plain text supported; should be enclosed in quotes\n" +
			"\t-o pdf - name of the PDF file to produce as output\n" +
			"\t     default is " + DEFAULT_OUTPUT + "\n" +
			"\t-s svc - location of the signing service\n" +
			"\t     default is " + ServiceConfiguration.DEFAULT_STAMPER + "\n" +
			"\t-i - create invisible signature\n" +
			"\t-L - display license information and acknowledgements\n" +
			"Each option should be given at most once";

	/**
	 * Usage message for the GUI.
	 */
	private static final String NOGUI =
			"This is a command-line tool with no graphical user interface.\n\n" +
			"Enter something along the lines of\n"+
			"        java -jar PdfBundler.jar -h\n" +
			"on a command line to get usage information.";

	/**
	 * License and acknowledgments message.
	 */
	private static final String LICENSE = TITLE + "\n\n" +
			"Copyright 2008-2011 GuardTime AS\n" +
			"Licensed under the GuardTime Tools End User Licensing Agreement\n" +
			"\thttp://www.guardtime.com/eula\n\n" +
			"Includes software developed by the Legion of the Bouncy Castle\n" +
			"\thttp://www.bouncycastle.org/\n\n" +
			"Includes software developed by Bruno Lowagie and contributors\n" +
			"\thttp://www.lowagie.com/iText/\n" +
			"Source code of iText 2.1.7 with our updates is available from\n" +
			"\thttp://download.guardtime.com/iText-2.1.7-fix-src.jar";

	/**
	 * Program entry point.
	 *
	 * @param args
	 *            command-line arguments, if any.
	 */
	public static void main(String[] args) {

		// if the user just double-clicks on the JAR file...
		if (args.length == 0) {
			PdfUtil.showMessage(NOGUI, TITLE);
			return;
		}

		// parse the command line
		String logo = null;
		String title = null;
		String name = null;
		String out = null;
		String url = null;
		boolean visible = true;
		ArrayList<String> files = new ArrayList<String>();
		for (int i = 0; i < args.length; ++i) {
			if ("-h".equals(args[i])) {
				System.err.println(USAGE);
			} else if ("-L".equals(args[i])) {
				System.err.println(LICENSE);
			} else if ("-i".equals(args[i])) {
				visible = false;
			} else if ("-s".equals(args[i])) {
				if (++i < args.length) {
					url = args[i];
				} else {
					System.err.println("-s must be followed by service URL");
				}
			} else if ("-o".equals(args[i])) {
				if (++i < args.length) {
					out = args[i];
				} else {
					System.err.println("-o must be followed by file name");
				}
			} else if ("-n".equals(args[i])) {
				if (++i < args.length) {
					name = args[i];
				} else {
					System.err.println("-n must be followed by the name");
				}
			} else if ("-t".equals(args[i])) {
				if (++i < args.length) {
					title = args[i];
				} else {
					System.err.println("-t must be followed by title text");
				}
			} else if ("-l".equals(args[i])) {
				if (++i < args.length) {
					logo = args[i];
				} else {
					System.err.println("-l must be followed by file name");
				}
			} else {
				files.add(args[i]);
			}
		}
		if (files.isEmpty()) {
			System.err.println("No files given, no bundle produced");
			return;
		}

		// set up service configuration
		ServiceConfiguration svc = null;
		try {
			svc = new ServiceConfiguration();
			if (url != null) {
				svc.setStamper(new URL(url));
			}
		} catch (Exception x) {
			System.err.println("Invalid service URL: " + x.getMessage());
			return;
		}

		// set up seal configuration
		SealConfiguration seal = null;
		if (visible) {
			seal = new SealConfiguration(PdfUtil.loadResource("/Seal.img"));
		}

		// create the bundle document
		ByteArrayOutputStream buf = new ByteArrayOutputStream();
		Document doc = new Document(PageSize.A4);
		PdfWriter writer = null;
		try {
			writer = PdfWriter.getInstance(doc, buf);
		} catch (Exception x) {
			System.err.println("Unexpected error while creating document: " + x.getMessage());
			return;
		}
		doc.open();

		// add the logo if requested
		if (logo != null) {
			Image img = null;
			try {
				if (logo.toLowerCase().endsWith(".pdf")) {
					// special handling for importing another PDF document as a logo image
					PdfReader reader = new PdfReader(logo);
					PdfImportedPage page = writer.getImportedPage(reader, 1);
					img = Image.getInstance(page);
				} else {
					img = Image.getInstance(logo);
				}
			} catch (Exception x) {
				System.err.println("Could not load the logo image: " + x.getMessage());
				return;
			}
			try {
				doc.add(img);
			} catch (Exception x) {
				System.err.println("Unexpected error while adding the logo to the document: " + x.getMessage());
				return;
			}
		}

		// add the title if requested
		if (title != null) {
			try {
				HTMLWorker html = new HTMLWorker(doc);
				html.parse(new StringReader(title));
			} catch (Exception x) {
				System.err.println("Unexpected error while adding the title to the document: " + x.getMessage());
				return;
			}
		}

		// create the file list
		for (Iterator<String> it = files.iterator(); it.hasNext(); ) {
			try {
				doc.add(new ListItem(it.next()));
			} catch (Exception x) {
				System.err.println("Unexpected error while adding text to the document: " + x.getMessage());
				return;
			}
		}

		// document body done
		doc.close();

		// create output file
		FileOutputStream os = null;
		try {
			if (out == null) {
				os = new FileOutputStream(DEFAULT_OUTPUT);
			} else {
				os = new FileOutputStream(out);
			}
		} catch (Exception x) {
			System.err.println("Could not create output file: " + x.getMessage());
			return;
		}

		// prepare to add attachments and sign
		PDFSigner sig = null;
		try {
			sig = new PDFSigner(new ByteArrayInputStream(buf.toByteArray()), os);
		} catch (Exception x) {
			System.err.println("Unexpected error while signing the document: " + x.getMessage());
			return;
		}

		// attach the files
		for (Iterator<String> it = files.iterator(); it.hasNext(); ) {
			String file = it.next();
			try {
				sig.addAttachment(file, new File(file));
			} catch (Exception x) {
				System.err.println("Could not add file " + file + " to the bundle: " + x.getMessage());
				return;
			}
		}

		// sign
		try {
			sig.sign(svc, seal, name);
		} catch (Exception x) {
			System.err.println("Could not sign the bundle: " + x.getMessage());
			return;
		}

	}

}
