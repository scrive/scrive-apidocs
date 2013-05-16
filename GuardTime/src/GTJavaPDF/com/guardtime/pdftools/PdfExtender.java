/*
 * $Id: PdfExtender.java 283 2013-03-28 16:42:48Z ahto.truu $
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

import java.io.File;
import java.net.URL;
import javax.swing.JFileChooser;

import com.guardtime.pdf.ServiceConfiguration;
import com.guardtime.pdf.SignedPDF;

/**
 * A sample utility to extend time-stamps in PDF files.
 */
public abstract class PdfExtender {

	/**
	 * The program title.
	 */
	private static final String TITLE = "GuardTime PDF Extending Tool v.0.3.2-scrive";

	/**
	 * Usage message for the CLI.
	 */
	private static final String USAGE = TITLE + "\n" +
			"Usage: java -jar PdfExtender.jar [-x url] [-f file] [-L] ...\n" +
			"\t-x - location of the extending (online verification) service\n" +
			"\t     default is " + ServiceConfiguration.DEFAULT_EXTENDER + "\n" +
			"\t-f - name of the file to be extended\n" +
			"\t     the signature in the file will be replaced with an extended one\n" +
			"\t-L - display license information and acknowledgements\n" +
			"The command line is scanned left to right\n" +
			"Each option takes effect as it is encountered\n" +
			"All options can be repeated as many times as desired\n" +
			"A simple GUI will be launched when no command line is given";

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
	 * The service configuration object.
	 */
	private static ServiceConfiguration svc;

	/**
	 * Program entry point.
	 *
	 * @param args
	 *            command-line arguments, if any.
	 */
	public static void main(String[] args) {
		try {
			svc = new ServiceConfiguration();
		} catch (Exception x) {
			PdfUtil.showMessage(x, TITLE);
			return;
		}

		if (args.length == 0) {
			runGui();
		} else {
			runCli(args);
		}
	}

	/**
	 * Runs a minimal GUI that allows the user to select one file, extend the
	 * time-stamp in it using the default service, and exits.
	 */
	private static void runGui() {
		JFileChooser jfc = new JFileChooser();
		jfc.setDialogTitle(TITLE);
		jfc.setApproveButtonText("Extend");
		jfc.setApproveButtonToolTipText("Extend selected file");
		jfc.setFileFilter(new PdfFileFilter());
		int res = jfc.showOpenDialog(null);
		if (res != JFileChooser.APPROVE_OPTION) {
			return;
		}

		try {
			File file = jfc.getSelectedFile();
			extendTimestamp(file);
			PdfUtil.showMessage(file + " extended", TITLE);
		} catch (Exception x) {
			PdfUtil.showMessage(x, TITLE);
		}
	}

	/**
	 * Runs a simple CLI that allows the user to indicate the files to extend
	 * and optionally the service to use.
	 */
	private static void runCli(String[] args) {
		for (int i = 0; i < args.length; ++i) {
			if ("-h".equals(args[i])) {
				System.err.println(USAGE);
			} else if ("-L".equals(args[i])) {
				System.err.println(LICENSE);
			} else if ("-f".equals(args[i])) {
				if (++i < args.length) {
					File file = new File(args[i]);
					try {
						extendTimestamp(file);
						System.out.println(file + " extended");
					} catch (Exception x) {
						System.err.println(file + " not extended: " + x);
					}
				} else {
					System.err.println("-f must be followed by file name");
				}
			} else if ("-x".equals(args[i])) {
				if (++i < args.length) {
					try {
						svc.setExtender(new URL(args[i]));
					} catch (Exception x) {
						System.err.println(x);
					}
				} else {
					System.err.println("-x must be followed by service URL");
				}
			} else {
				System.err.println("Unknown option " + args[i] + " ignored. Use -h for help.");
			}
		}
	}

	/**
	 * Internal helper to extend the time-stamp in one file.
	 *
	 * @param file
	 *            the file to extend.
	 *
	 * @throws Exception
	 *             on any errors.
	 */
	private static void extendTimestamp(File file)
	throws Exception {
		SignedPDF doc = new SignedPDF(file);
		doc.extendTimestamp(svc);
		doc.write(file);
	}

}
