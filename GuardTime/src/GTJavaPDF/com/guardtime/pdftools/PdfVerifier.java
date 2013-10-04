/*
 * $Id: PdfVerifier.java 318 2013-10-03 22:50:16Z ahto.truu $
 *
 * Copyright 2008-2013 Guardtime AS
 *
 * This file is part of the Guardtime PDF Toolkit, an addendum
 * to the Guardtime Client SDK for Java.
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
import java.util.Iterator;
import java.util.List;
import javax.swing.JFileChooser;

import com.guardtime.pdf.ServiceConfiguration;
import com.guardtime.pdf.SignedPDF;
import com.guardtime.tsp.GTTimestamp;
import com.guardtime.tsp.GTVerificationResult;
import com.guardtime.tsp.VerificationResult;

/**
 * A sample utility to verify time-stamps in PDF files.
 */
public abstract class PdfVerifier {

	/**
	 * The program title.
	 */
	private static final String TITLE = "Guardtime PDF Verification Tool v.0.3.4";

	/**
	 * Usage message for the CLI.
	 */
	private static final String USAGE = TITLE + "\n" +
			"Usage: java -jar PdfVerifier.jar [-x url] [-p url] [-j] [-f file] [-L] ...\n" +
			"\t-x - location of the extending (online verification) service\n" +
			"\t     default is " + ServiceConfiguration.DEFAULT_EXTENDER + "\n" +
			"\t     specify '-x -' for no extending service access\n" +
			"\t-p - location of the control publications (Integrity Codes)\n" +
			"\t     default is " + ServiceConfiguration.DEFAULT_PUBLICATIONS + "\n" +
			"\t-j - verification result will be printed in JSON\n" +
			"\t     only the last Guardtime signature will be verified in JSON mode\n" +
			"\t-f - name of the file to be verified\n" +
			"\t     all Guardtime signatures in the file will be verified\n" +
			"\t-L - display license information and acknowledgements\n" +
			"The command line is scanned left to right\n" +
			"Each option takes effect as it is encountered\n" +
			"All options can be repeated as many times as desired\n" +
			"Each use of -x, -p overrides the previous one\n" +
			"A simple GUI will be launched when no command line is given\n" +
			"The GUI only verifies the last Guardtime signature in the file";

	/**
	 * License and acknowledgments message.
	 */
	private static final String LICENSE = TITLE + "\n\n" +
			"Copyright 2008-2013 Guardtime AS\n" +
			"Licensed under the Guardtime Tools End User Licensing Agreement\n" +
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
	 * Runs a minimal GUI that allows the user to select one file, verifies the
	 * most recent time-stamp in it, and exits.
	 */
	private static void runGui() {
		JFileChooser jfc = new JFileChooser();
		jfc.setDialogTitle(TITLE);
		jfc.setApproveButtonText("Verify");
		jfc.setApproveButtonToolTipText("Verify selected file");
		jfc.setFileFilter(new PdfFileFilter());
		int res = jfc.showOpenDialog(null);
		if (res != JFileChooser.APPROVE_OPTION) {
			return;
		}

		try {
			File file = jfc.getSelectedFile();
			SignedPDF doc = new SignedPDF(file);
			PdfUtil.showMessage(file + verifyTimestamp(doc, doc.getTimestamp()), TITLE);
		} catch (Exception x) {
			PdfUtil.showMessage(x, TITLE);
		}
	}

	/**
	 * Runs a simple CLI that allows the user to indicate the files to verify
	 * and optionally the service to use.
	 */
	private static void runCli(String[] args) {

		boolean json = false;

		for (int i = 0; i < args.length; ++i) {
			if ("-h".equals(args[i])) {
				System.err.println(USAGE);
			} else if ("-L".equals(args[i])) {
				System.err.println(LICENSE);
			} else if ("-f".equals(args[i])) {
				if (++i < args.length) {
					File file = new File(args[i]);
					try {
						SignedPDF doc = new SignedPDF(file);
						if (json) {
							System.out.print(verifyTimestampJSON(doc, doc.getTimestamp()));
						} else {
							System.out.print(file + ":");
							List<SignedPDF.SignatureInfo> sig = doc.getTimestamps();
							for (Iterator<SignedPDF.SignatureInfo> it = sig.iterator(); it.hasNext(); ) {
								System.out.print(verifyTimestamp(doc, it.next()));
							}
							System.out.println();
						}
					} catch (Exception x) {
						if (json){
							System.err.println("{\n  \"error\" : {\n    \"reason\": \""+x.toString()+"\"\n  }\n}\n");
						} else {
							System.err.println(file + ":" + x.toString());
						}
					}
				} else {
					System.err.println("-f must be followed by file name");
				}
			} else if ("-x".equals(args[i])) {
				if (++i < args.length) {
					if ("-".equals(args[i])) {
						svc.setExtender(null);
					} else {
						try {
							svc.setExtender(new URL(args[i]));
						} catch (Exception x) {
							System.err.println(x);
						}
					}
				} else {
					System.err.println("-x must be followed by service URL or '-'");
				}
			} else if ("-j".equals(args[i])) {
				json = true;
			} else if ("-p".equals(args[i])) {
				if (++i < args.length) {
					try {
						svc.setPublications(new URL(args[i]));
					} catch (Exception x) {
						System.err.println(x);
					}
				} else {
					System.err.println("-p must be followed by publications file URL");
				}
			} else {
				System.err.println("Unknown option " + args[i] + " ignored. Use -h for help.");
			}
		}
	}

	/**
	 * Internal helper to verify one time-stamp.
	 *
	 * @param doc
	 *            the document that contains the time-stamp.
	 * @param si
	 *            the time-stamp to verify.
	 * @return verification result as a String
	 *
	 * @throws Exception
	 *             on any errors.
	 */
	private static String verifyTimestamp(SignedPDF doc, SignedPDF.SignatureInfo si)
	throws Exception {
		if (si == null) {
			return "\n\tNOT verified: no Guardtime signatures in the document";
		}
		StringBuffer res = new StringBuffer();
		VerificationResult ver = doc.verifyTimestamp(si, svc);
		if (ver.isValid()) {
			GTTimestamp ts = si.getTimestamp();
			String time = PdfUtil.toLocalString(ts.getRegisteredTime());
			String gwid = PdfUtil.formatGwId(ts.getProperty(GTTimestamp.LOCATION_ID));
			String gwname = ts.getProperty(GTTimestamp.LOCATION_NAME);
			res.append("\n\t").append(si.getName()).append(" verified:");
			res.append("\n\t\tsigned at ").append(time);
			if (gwname == null) {
				res.append("\n\t\tby ").append(gwid);
			} else {
				res.append("\n\t\tby ").append(gwname).append(" (").append(gwid).append(")");
			}
			if (si.isWhole()) {
				res.append("\n\t\tand has not been modified since");
			} else {
				res.append("\n\t\tbut has been revised since");
			}
		} else {
			res.append("\n\t").append(si.getName()).append(" NOT verified:");
			boolean first = true;
			if (ver.hasError(GTVerificationResult.SYNTACTIC_CHECK_FAILURE)) {
				res.append(first ? " " : ", ");
				res.append("SYNTACTIC_CHECK_FAILURE");
				first = false;
			}
			if (ver.hasError(GTVerificationResult.HASHCHAIN_VERIFICATION_FAILURE)) {
				res.append(first ? " " : ", ");
				res.append("HASHCHAIN_VERIFICATION_FAILURE");
				first = false;
			}
			if (ver.hasError(GTVerificationResult.PUBLIC_KEY_SIGNATURE_FAILURE)) {
				res.append(first ? " " : ", ");
				res.append("PUBLIC_KEY_SIGNATURE_FAILURE");
				first = false;
			}
			if (ver.hasError(GTVerificationResult.PUBLIC_KEY_FAILURE)) {
				res.append(first ? " " : ", ");
				res.append("PUBLIC_KEY_FAILURE");
				first = false;
			}
			if (ver.hasError(GTVerificationResult.WRONG_DOCUMENT_FAILURE)) {
				res.append(first ? " " : ", ");
				res.append("WRONG_DOCUMENT_FAILURE");
				first = false;
			}
			if (ver.hasError(GTVerificationResult.PUBLICATION_FAILURE)) {
				res.append(first ? " " : ", ");
				res.append("PUBLICATION_FAILURE");
				first = false;
			}
			if (ver.hasError(GTVerificationResult.CERTIFICATE_FAILURE)) {
				res.append(first ? " " : ", ");
				res.append("CERTIFICATE_FAILURE");
				first = false;
			}
			if (ver.hasError(GTVerificationResult.TECH_FAILURE)) {
				res.append(first ? " " : ", ");
				res.append("TECH_FAILURE");
				first = false;
			}
			if (ver.hasError(GTVerificationResult.PUBFILE_SIGNATURE_FAILURE)) {
				res.append(first ? " " : ", ");
				res.append("PUBFILE_SIGNATURE_FAILURE");
				first = false;
			}
		}
		return res.toString();
	}


	/**
	 * Internal helper to verify one time-stamp.
	 *
	 * @param doc
	 *            the document that contains the time-stamp.
	 * @param si
	 *            the time-stamp to verify.
	 *
	 * @return verification result as a JSON-formatted String
	 *
	 * @throws Exception
	 *             on any errors.
	 *
	 * XXX: JSON should not be created this way.
	 */
	private static String verifyTimestampJSON(SignedPDF doc, SignedPDF.SignatureInfo si)
	throws Exception {
		if (si == null) {
			return "{\n  \"invalid\" : {\n    \"reason\": \"no Guardtime signatures in the document\"\n  }\n}\n";
		}
		StringBuffer res = new StringBuffer();
		boolean extended = si.getTimestamp().isExtended();
		VerificationResult ver = doc.verifyTimestamp(si, svc);
		if (ver.isValid()) {
			GTTimestamp ts = si.getTimestamp();
			String time = PdfUtil.toUtcString(ts.getRegisteredTime());
			String gwid = PdfUtil.formatGwId(ts.getProperty(GTTimestamp.LOCATION_ID));
			String gwname = ts.getProperty(GTTimestamp.LOCATION_NAME);
			String pubtime = ts.getProperty(GTTimestamp.PUBLICATION_TIME);
			res.append("{\n  \"valid\": {\n");
			res.append("    \"time\": \"").append(time).append("\",\n");
			res.append("    \"gateway_id\": \"").append(gwid).append("\",\n");
			if (gwname != null) {
				res.append("    \"gateway_name\": \"").append(gwname).append("\",\n");
			}
			res.append("    \"extended\": ").append(extended).append(",\n");
			res.append("    \"extensible\": ").append(!extended && ts.isExtended()).append(",\n");
			if (extended && pubtime != null) {
				res.append("    \"publication_time\": \"").append(pubtime).append("\",\n");
			}
			res.append("    \"last_revision\": \"").append(si.isWhole()).append("\"");
			res.append("\n  }\n}\n");
		} else {
			res.append("{\n  \"invalid\" : {");
			res.append("\n    \"reason\":");
			boolean first = true;
			if (ver.hasError(GTVerificationResult.SYNTACTIC_CHECK_FAILURE)) {
				res.append(first ? " " : ", ");
				res.append("\"SYNTACTIC_CHECK_FAILURE\"");
				first = false;
			}
			if (ver.hasError(GTVerificationResult.HASHCHAIN_VERIFICATION_FAILURE)) {
				res.append(first ? " " : ", ");
				res.append("\"HASHCHAIN_VERIFICATION_FAILURE\"");
				first = false;
			}
			if (ver.hasError(GTVerificationResult.PUBLIC_KEY_SIGNATURE_FAILURE)) {
				res.append(first ? " " : ", ");
				res.append("\"PUBLIC_KEY_SIGNATURE_FAILURE\"");
				first = false;
			}
			if (ver.hasError(GTVerificationResult.PUBLIC_KEY_FAILURE)) {
				res.append(first ? " " : ", ");
				res.append("\"PUBLIC_KEY_FAILURE\"");
				first = false;
			}
			if (ver.hasError(GTVerificationResult.WRONG_DOCUMENT_FAILURE)) {
				res.append(first ? " " : ", ");
				res.append("\"WRONG_DOCUMENT_FAILURE\"");
				first = false;
			}
			if (ver.hasError(GTVerificationResult.PUBLICATION_FAILURE)) {
				res.append(first ? " " : ", ");
				res.append("\"PUBLICATION_FAILURE\"");
				first = false;
			}
			if (ver.hasError(GTVerificationResult.CERTIFICATE_FAILURE)) {
				res.append(first ? " " : ", ");
				res.append("\"CERTIFICATE_FAILURE\"");
				first = false;
			}
			if (ver.hasError(GTVerificationResult.TECH_FAILURE)) {
				res.append(first ? " " : ", ");
				res.append("\"TECH_FAILURE\"");
				first = false;
			}
			if (ver.hasError(GTVerificationResult.PUBFILE_SIGNATURE_FAILURE)) {
				res.append(first ? " " : ", ");
				res.append("\"PUBFILE_SIGNATURE_FAILURE\"");
				first = false;
			}
			res.append("\n  }\n}\n");
		}
		return res.toString();
	}

}
