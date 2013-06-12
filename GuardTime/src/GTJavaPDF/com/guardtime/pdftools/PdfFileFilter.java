/*
 * $Id: PdfFileFilter.java 177 2011-04-01 19:10:34Z ahto.truu $
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
import javax.swing.filechooser.FileFilter;

/**
 * A helper class to allow the GUI utilities to list only PDF files.
 */
public class PdfFileFilter
extends FileFilter {

	public boolean accept(File f) {
		return f.isDirectory() || f.getName().toLowerCase().endsWith(".pdf");
	}

	public String getDescription() {
		return "Portable Document Format files (*.pdf)";
	}

}
