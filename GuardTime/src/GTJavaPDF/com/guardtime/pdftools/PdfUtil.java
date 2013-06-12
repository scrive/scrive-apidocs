/*
 * $Id: PdfUtil.java 244 2012-02-09 12:11:30Z ahto.truu $
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
import java.io.FileInputStream;
import java.io.IOException;
import java.text.DateFormat;
import java.util.Date;
import java.util.TimeZone;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JOptionPane;

import com.guardtime.util.Util;

/**
 * An abstract class containing static helper methods to load data from files
 * and resources and format and display messages.
 */
public abstract class PdfUtil {

	/**
	 * The icon for message boxes.
	 */
	private static Icon icon = null;

	/**
	 * A date formatter object initialized to UTC time zone.
	 */
	private static DateFormat utcFormat = DateFormat.getDateTimeInstance(DateFormat.LONG, DateFormat.LONG);
	{
		utcFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
	}

	/**
	 * A date formatter object initialized to local time zone.
	 */
	private static DateFormat localFormat = DateFormat.getDateTimeInstance(DateFormat.LONG, DateFormat.LONG);

	/**
	 * Converts a date to a string according to the UTC time zone.
	 * 
	 * @param date
	 *            the date to convert.
	 * 
	 * @return string representation of {@code date}.
	 */
	public static String toUtcString(Date date) {
		return utcFormat.format(date);
	}

	/**
	 * Converts a date to a string according to the local time zone.
	 * 
	 * @param date
	 *            the date to convert.
	 * 
	 * @return string representation of {@code date}.
	 */
	public static String toLocalString(Date date) {
		return localFormat.format(date);
	}

	/**
	 * Formats a GuardTime Gateway identifier.
	 * 
	 * @param gwid
	 *            the identifier to format.
	 * 
	 * @return formatted identifier.
	 */
	public static String formatGwId(String gwid) {
		try {
			long id = Long.parseLong(gwid);
			StringBuffer res = new StringBuffer();
			res.append(id >>> 48 & 0xffff);
			res.append(".");
			res.append(id >>> 32 & 0xffff);
			res.append(".");
			res.append(id >>> 16 & 0xffff);
			res.append(".");
			res.append(id & 0xffff);
			return res.toString();
		} catch (NullPointerException x) {
			return null;
		} catch (NumberFormatException x) {
			return null;
		}
	}

	/**
	 * Displays a message box.
	 * 
	 * @param msg
	 *            the message to show.
	 * @param title
	 *            the title for the message box.
	 */
	static void showMessage(String msg, String title) {
		if (icon == null) {
			// lazy initialization to avoid exceptions on headless systems
			icon = new ImageIcon(loadResource("/Icon.img"));
		}
		JOptionPane.showMessageDialog(null, msg, title, JOptionPane.PLAIN_MESSAGE, icon);
	}

	/**
	 * Displays an error message from the given exception.
	 * 
	 * @param x
	 *            the exception to display.
	 * @param title
	 *            the title for the message box.
	 */
	static void showMessage(Exception x, String title) {
		String msg = x.getMessage();
		if (msg == null) {
			msg = x.toString();
		}
		showMessage("Unexpected error:\n" + msg, title);
	}

	/**
	 * Loads the contents of a file.
	 * 
	 * @param file
	 *            the file.
	 * @return the loaded data as a byte array, or {@code null} on error.
	 */
	public static byte[] loadFile(File file) {
		try {
			return Util.readAll(new FileInputStream(file));
		} catch (IOException x) {
			return null;
		}
	}

	/**
	 * Loads the contents of a resource.
	 * 
	 * @param name
	 *            the name of the recourse.
	 * @return the loaded data as a byte array, or {@code null} on error.
	 */
	public static byte[] loadResource(String name) {
		try {
			return Util.readAll(Object.class.getResourceAsStream(name));
		} catch (IOException x) {
			return null;
		}
	}

}
