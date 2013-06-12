/*
 * $Id: LazyFileOutputStream.java 234 2011-12-19 22:37:50Z ahto.truu $
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
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;

/**
 * Implements a wrapper around standard {@code FileOutputStream} with the
 * property that the file is not actually created until it is written to.
 */
class LazyFileOutputStream extends OutputStream {

	/**
	 * Name of the file to write to.
	 */
	private File file;

	/**
	 * The embedded {@code FileOutputStream} that will do the actual writing.
	 */
	private FileOutputStream os = null;

	/**
	 * Prepares to write to the given file, but does not create the file yet.
	 * 
	 * @param name
	 *            name of the file to write to.
	 */
	LazyFileOutputStream(String name) {
		this(new File(name));
	}

	/**
	 * Prepares to write to the given file, but does not create the file yet.
	 * 
	 * @param file
	 *            the file to write to.
	 */
	LazyFileOutputStream(File file) {
		this.file = file;
	}

	/**
	 * Writes the given byte to this stream.
	 * <p>
	 * As the file is created the first time it is written to, this method may
	 * throw the {@code SecurityException} and {@code FileNotFoundException}
	 * exceptions that a normal {@code FileOutputStream} would throw from a
	 * constructor.
	 * 
	 * @param b
	 *            the byte to be written.
	 * 
	 * @throws SecurityException
	 *             if a security manager denies write access to the file.
	 * @throws FileNotFoundException
	 *             if the file cannot be opened for any other reason.
	 * @throws IOException
	 *             if any other I/O error occurs.
	 */
	public void write(int b)
	throws SecurityException, FileNotFoundException, IOException {
		check();
		os.write(b);
	}

	/**
	 * Writes the contents of the given byte array to this stream.
	 * <p>
	 * As the file is created the first time it is written to, this method may
	 * throw the {@code SecurityException} and {@code FileNotFoundException}
	 * exceptions that a normal {@code FileOutputStream} would throw from a
	 * constructor.
	 * 
	 * @param b
	 *            the data to be written.
	 * 
	 * @throws SecurityException
	 *             if a security manager denies write access to the file.
	 * @throws FileNotFoundException
	 *             if the file cannot be opened for any other reason.
	 * @throws IOException
	 *             if any other I/O error occurs.
	 */
	public void write(byte[] b)
	throws SecurityException, FileNotFoundException, IOException {
		check();
		os.write(b);
	}

	/**
	 * Writes the specified section of the given byte array to this stream.
	 * <p>
	 * As the file is created the first time it is written to, this method may
	 * throw the {@code SecurityException} and {@code FileNotFoundException}
	 * exceptions that a normal {@code FileOutputStream} would throw from a
	 * constructor.
	 * 
	 * @param b
	 *            the data to be written.
	 * @param off
	 *            the start offset in the data.
	 * @param len
	 *            the number of bytes to write.
	 * 
	 * @throws SecurityException
	 *             if a security manager denies write access to the file.
	 * @throws FileNotFoundException
	 *             if the file cannot be opened for any other reason.
	 * @throws IOException
	 *             if any other I/O error occurs.
	 */
	public void write(byte[] b, int off, int len)
	throws SecurityException, FileNotFoundException, IOException {
		check();
		os.write(b, off, len);
	}

	/**
	 * Flushes this stream.
	 * <p>
	 * If the file has not been created yet, this is done now to ensure that the
	 * end result is an empty file just as with creating and flushing a standard
	 * {@code FileOutputStream} object. Consequently, this method may throw the
	 * {@code SecurityException} and {@code FileNotFoundException} exceptions
	 * that a normal {@code FileOutputStream} would throw from a constructor.
	 * 
	 * @throws SecurityException
	 *             if a security manager denies write access to the file.
	 * @throws FileNotFoundException
	 *             if the file cannot be opened for any other reason.
	 * @throws IOException
	 *             if any other I/O error occurs.
	 */
	public void flush()
	throws SecurityException, FileNotFoundException, IOException {
		check();
		os.flush();
	}

	/**
	 * Closes this stream.
	 * <p>
	 * If the file has not been created yet, this is done now to ensure that the
	 * end result is an empty file just as with creating and closing a standard
	 * {@code FileOutputStream} object. Consequently, this method may throw the
	 * {@code SecurityException} and {@code FileNotFoundException} exceptions
	 * that a normal {@code FileOutputStream} would throw from a constructor.
	 * 
	 * @throws SecurityException
	 *             if a security manager denies write access to the file.
	 * @throws FileNotFoundException
	 *             if the file cannot be opened for any other reason.
	 * @throws IOException
	 *             if any other I/O error occurs.
	 */
	public void close()
	throws SecurityException, FileNotFoundException, IOException {
		check();
		os.close();
	}

	/**
	 * Internal helper to create the embedded {@code FileOutputStream} object if
	 * it does not exist already.
	 * 
	 * @throws SecurityException
	 *             if a security manager denies write access to the file.
	 * @throws FileNotFoundException
	 *             if the file cannot be opened for any other reason.
	 */
	private synchronized void check()
	throws SecurityException, FileNotFoundException {
		if (os == null) {
			os = new FileOutputStream(file);
		}
	}

}
