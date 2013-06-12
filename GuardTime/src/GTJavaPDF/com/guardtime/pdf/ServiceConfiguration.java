/*
 * $Id: ServiceConfiguration.java 246 2012-02-25 21:53:38Z ahto.truu $
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

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;

import com.guardtime.transport.SimpleHttpStamper;
import com.guardtime.tsp.GTException;
import com.guardtime.tsp.GTPublicationsFile;
import com.guardtime.tsp.GTVerificationResult;

/**
 * Represents the configuration information needed to access the GuardTime
 * service.
 */
public class ServiceConfiguration {

	/**
	 * The default time-stamping service.
	 * <p>
	 * Note that this is the global public service provided by GuardTime for
	 * integration testing on "as-is" basis. Please contact GuardTime for a
	 * commercial service if you require any availability guarantees for a
	 * production deployment.
	 * 
	 * @see #setStamper(URL)
	 * @see #getStamper()
	 */
	public static final String DEFAULT_STAMPER = "http://stamper.guardtime.net/gt-signingservice";

	/**
	 * The default extending (online verification) service.
	 * <p>
	 * Note that this is the global public service provided by GuardTime for
	 * integration testing on "as-is" basis. Please contact GuardTime for a
	 * commercial service if you require any availability guarantees for a
	 * production deployment.
	 * 
	 * @see #setExtender(URL)
	 * @see #getExtender()
	 */
	public static final String DEFAULT_EXTENDER = "http://verifier.guardtime.net/gt-extendingservice";

	/**
	 * The default location of the electronic archive of control publications
	 * (Integrity Codes).
	 * 
	 * @see #setPublications(URL)
	 * @see #getPublications()
	 */
	public static final String DEFAULT_PUBLICATIONS = "http://verify.guardtime.com/gt-controlpublications.bin";

	/**
	 * The default value for maximum allowed age of the locally cached copy of
	 * the electronic archive of control publications (Integrity Codes).
	 * <p>
	 * The value is given in milliseconds.
	 * 
	 * @see #setPublicationsAgeLimit(int)
	 */
	public static final long DEFAULT_PUBLICATIONS_AGE_LIMIT = 3600 * 1000L;

	/**
	 * Location of the time-stamping service.
	 * 
	 * @see #setStamper(URL)
	 * @see #getStamper()
	 */
	private URL stamper;

	/**
	 * Location of the extending (online verification) service.
	 * 
	 * @see #setExtender(URL)
	 * @see #getExtender()
	 */
	private URL extender;

	/**
	 * Location of the electronic archive of control publications (Integrity
	 * Codes).
	 * 
	 * @see #setPublications(URL)
	 * @see #setPublicationsAgeLimit(int)
	 * @see #getPublications()
	 */
	private URL publications;

	/**
	 * Cached copy of the electronic archive of control publications (Integrity
	 * Codes).
	 * 
	 * @see #setPublications(URL)
	 * @see #setPublicationsAgeLimit(int)
	 * @see #getPublications()
	 */
	private GTPublicationsFile publicationsFile = null;

	/**
	 * The time when the currently cached copy of the electronic archive of
	 * control publications (Integrity Codes) was downloaded.
	 * <p>
	 * The value is stored in milliseconds since 1970-01-01 00:00:00 UTC.
	 * 
	 * @see #setPublications(URL)
	 * @see #setPublicationsAgeLimit(int)
	 * @see #getPublications()
	 */
	private long publicationsTime = 0;

	/**
	 * The maximal allowed age of the cached copy of the electronic archive of
	 * control publications (Integrity Codes).
	 * <p>
	 * The value is stored in milliseconds.
	 * 
	 * @see #setPublications(URL)
	 * @see #setPublicationsAgeLimit(int)
	 * @see #getPublications()
	 */
	private long publicationsAgeLimit = DEFAULT_PUBLICATIONS_AGE_LIMIT;

	/**
	 * Creates a new configuration with the default values.
	 */
	public ServiceConfiguration()
	throws MalformedURLException {
		this(new URL(DEFAULT_STAMPER), new URL(DEFAULT_EXTENDER), new URL(DEFAULT_PUBLICATIONS));
	}

	/**
	 * Creates a new configuration with the given values.
	 * 
	 * @param stamper
	 *            location of the time-stamping service.
	 * @param extender
	 *            location of the extending (online verification) service.
	 * @param publications
	 *            location of the electronic archive of control publications
	 *            (Integrity Codes).
	 */
	public ServiceConfiguration(URL stamper, URL extender, URL publications) {
		setStamper(stamper);
		setExtender(extender);
		setPublications(publications);
	}

	/**
	 * Sets the location of the time-stamping service to use.
	 * 
	 * @param stamper
	 *            the location of the time-stamping service to use.
	 * 
	 * @see #getStamper()
	 */
	public void setStamper(URL stamper) {
		this.stamper = stamper;
	}

	/**
	 * Returns the location of the time-stamping service to use.
	 * 
	 * @return the location of the time-stamping service to use.
	 * 
	 * @see #setStamper(URL)
	 */
	public URL getStamper() {
		return stamper;
	}

	/**
	 * Sets the location of the extending (online verification) service to use.
	 * 
	 * @param extender
	 *            the location of the extending service to use.
	 * 
	 * @see #getExtender()
	 */
	public void setExtender(URL extender) {
		this.extender = extender;
	}

	/**
	 * Returns the location of the extending (online verification) service to
	 * use.
	 * 
	 * @return the location of the extending service to use.
	 * 
	 * @see #setExtender(URL)
	 */
	public URL getExtender() {
		return extender;
	}

	/**
	 * Sets the location of the electronic archive of control publications
	 * (Integrity Codes) to use.
	 * 
	 * @param publications
	 *            the location of the control publications to use.
	 * 
	 * @see #setPublicationsAgeLimit(int)
	 * @see #getPublications()
	 */
	public void setPublications(URL publications) {
		this.publications = publications;
		this.publicationsFile = null;
	}

	/**
	 * Set the maximal allowed age of the cached copy of the electronic archive
	 * of control publications (Integrity Codes).
	 * 
	 * @param limit
	 *            the maximal allowed age, in seconds.
	 * 
	 * @see #setPublications(URL)
	 * @see #getPublications()
	 */
	public void setPublicationsAgeLimit(int limit) {
		this.publicationsAgeLimit = limit * 1000L;
	}

	/**
	 * Returns the electronic archive of control publications (Integrity Codes).
	 * <p>
	 * Automatically downloads and verifies the file if there is no cached copy
	 * or if the cached copy it too old.
	 * 
	 * @return the control publications.
	 * 
	 * @throws IOException
	 *             when there are errors downloading the publications file.
	 * @throws GTException
	 *             when the dowloaded publications file fails verification.
	 * 
	 * @see #setPublications(URL)
	 * @see #setPublicationsAgeLimit(int)
	 */
	public GTPublicationsFile getPublications()
	throws IOException, GTException {
		if (this.publicationsFile == null || System.currentTimeMillis() > this.publicationsTime + this.publicationsAgeLimit) {
			GTPublicationsFile pub = SimpleHttpStamper.getPublicationsFile(this.publications);
			GTVerificationResult res = pub.verifySignature();
			if (!res.isValid()) {
				throw new GTException("Invalid publications file");
			}
			this.publicationsFile = pub;
			this.publicationsTime = System.currentTimeMillis();
		}
		return this.publicationsFile;
	}

}
