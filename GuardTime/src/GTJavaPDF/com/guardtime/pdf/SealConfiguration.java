/*
 * $Id: SealConfiguration.java 244 2012-02-09 12:11:30Z ahto.truu $
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

/**
 * Represents the information needed to add a visual "seal" image to the
 * documents as they are time-stamped.
 */
public class SealConfiguration {

	/**
	 * The "seal" image.
	 * 
	 * @see #setImage(byte[])
	 * @see #getImage()
	 */
	private byte[] image;

	/**
	 * The page to which the "seal" image is to be added.
	 * 
	 * @see #setPage(int)
	 * @see #getPage()
	 */
	private int page;

	/**
	 * The horizontal position of the "seal" image on the page.
	 * 
	 * @see #setX(float)
	 * @see #getX()
	 */
	private float x;

	/**
	 * The vertical position of the "seal" image on the page.
	 * 
	 * @see #setY(float)
	 * @see #getY()
	 */
	private float y;

	/**
	 * The width of the "seal" image on the page.
	 * 
	 * @see #setWidth(float)
	 * @see #getWidth()
	 */
	private float width;

	/**
	 * The height of the "seal" image on the page.
	 * 
	 * @see #setHeight(float)
	 * @see #getHeight()
	 */
	private float height;

	/**
	 * Creates a new "seal" to be displayed in the default position (top right
	 * corner of the first page of the document).
	 * 
	 * @param image
	 *            the image data.
	 */
	public SealConfiguration(byte[] image) {
		this(image, 1, mmToPt(-5), mmToPt(5), mmToPt(30), 0);
	}

	/**
	 * Creates a new "seal" with the given settings.
	 * 
	 * @param image
	 *            the image data.
	 * @param page
	 *            the page to which the image is to be added.
	 * @param x
	 *            horizontal position of the image on the page.
	 * @param y
	 *            vertical position of the image on the page.
	 * @param width
	 *            width of the image on the page.
	 * @param height
	 *            height of the image on the page.
	 */
	public SealConfiguration(byte[] image, int page, float x, float y, float width, float height) {
		setImage(image);
		setPage(page);
		setPosition(x, y);
		setSize(width, height);
	}

	/**
	 * Sets the "seal" image.
	 * <p>
	 * Note that the image is given as a byte array, not as an AWT {@code Image}
	 * object. This is to be able to support more image types, most notably the
	 * vector formats that the AWT class does not handle.
	 * 
	 * @param image
	 *            the image data.
	 * 
	 * @see #getImage()
	 */
	public void setImage(byte[] image) {
		this.image = image;
	}

	/**
	 * Gets the "seal" image.
	 * 
	 * @return the image data.
	 * 
	 * @see #setImage(byte[])
	 */
	public byte[] getImage() {
		return image;
	}

	/**
	 * Sets the page to which the "seal" image is to be added.
	 * <p>
	 * A positive value means counting forward from the beginning of the
	 * document. A negative value means counting backward from the end of the
	 * document. Zero or a value whose magnitude exceeds the number of pages in
	 * the document is an error.
	 * 
	 * @param page
	 *            the page indicator.
	 * 
	 * @see #getPage()
	 */
	public void setPage(int page) {
		this.page = page;
	}

	/**
	 * Gets the page to which the "seal" image is to be added.
	 * 
	 * @return the page indicator.
	 * 
	 * @see #setPage(int)
	 */
	public int getPage() {
		return page;
	}

	/**
	 * Sets the position of the "seal" image on the page.
	 * <p>
	 * For the horizontal position, a positive value is the distance from the
	 * left edge of the page to the left side of the image. A negative value is
	 * the distance from the right edge of the page to the right side of the
	 * image. Zero means centering. A value pushing the image off the page is an
	 * error.
	 * <p>
	 * For the vertical position, a positive value is the distance from the top
	 * of the page to the top of the image. A negative value is the distance
	 * from the bottom of the page to the bottom of the image. Zero means
	 * centering. A value pushing the image off the page is an error.
	 * <p>
	 * The position must be given in PostScript points (1 pt = 1/72 in = 25.4/72
	 * mm). You can use {@link #mmToPt(float)} and {@link #inToPt(float)} to
	 * convert from millimeters and inches to points.
	 * 
	 * @param x
	 *            the horizontal position, in points.
	 * @param y
	 *            the vertical position, in points.
	 * 
	 * @see #setX(float)
	 * @see #setY(float)
	 */
	public void setPosition(float x, float y) {
		setX(x);
		setY(y);
	}

	/**
	 * Sets the horizontal position of the "seal" image on the page.
	 * <p>
	 * A positive value is the distance from the left edge of the page to the
	 * left side of the image. A negative value is the distance from the right
	 * edge of the page to the right side of the image. Zero means centering. A
	 * value pushing the image off the page is an error.
	 * <p>
	 * The position must be given in PostScript points (1 pt = 1/72 in = 25.4/72
	 * mm). You can use {@link #mmToPt(float)} and {@link #inToPt(float)} to
	 * convert from millimeters and inches to points.
	 * 
	 * @param x
	 *            the horizontal position, in points.
	 * 
	 * @see #setPosition(float, float)
	 * @see #getX()
	 */
	public void setX(float x) {
		this.x = x;
	}

	/**
	 * Gets the horizontal position of the "seal" image on the page.
	 * <p>
	 * The position is returned in PostScript points (1 pt = 1/72 in = 25.4/72
	 * mm). You can use {@link #ptToMm(float)} and {@link #ptToIn(float)} to
	 * convert from points to millimeters and inches.
	 * 
	 * @return the horizontal position, in points.
	 * 
	 * @see #setPosition(float, float)
	 * @see #setX(float)
	 */
	public float getX() {
		return x;
	}

	/**
	 * Sets the vertical position of the "seal" image on the page.
	 * <p>
	 * A positive value is the distance from the top of the page to the top of
	 * the image. A negative value is the distance from the bottom of the page
	 * to the bottom of the image. Zero means centering. A value pushing the
	 * image off the page is an error.
	 * <p>
	 * The position must be given in PostScript points (1 pt = 1/72 in = 25.4/72
	 * mm). You can use {@link #mmToPt(float)} and {@link #inToPt(float)} to
	 * convert from millimeters and inches to points.
	 * 
	 * @param y
	 *            the vertical position, in points.
	 * 
	 * @see #setPosition(float, float)
	 * @see #getY()
	 */
	public void setY(float y) {
		this.y = y;
	}

	/**
	 * Gets the vertical position of the "seal" image on the page.
	 * <p>
	 * The position is returned in PostScript points (1 pt = 1/72 in = 25.4/72
	 * mm). You can use {@link #ptToMm(float)} and {@link #ptToIn(float)} to
	 * convert from points to millimeters and inches.
	 * 
	 * @return the vertical position, in points.
	 * 
	 * @see #setPosition(float, float)
	 * @see #setY(float)
	 */
	public float getY() {
		return y;
	}

	/**
	 * Sets the desired size of the "seal" image on the page.
	 * <p>
	 * If both the width and height are set to zero, the image is used in its
	 * original size. If both have positive values, the image is forced to the
	 * desired size, possibly distorting it in the process. If one has a
	 * positive value and the other is zero, the image is scaled proportionally
	 * to have the desired size in the dimension with the positive value. A
	 * negative value in either dimension is an error.
	 * <p>
	 * The sizes must be given in PostScript points (1 pt = 1/72 in = 25.4/72
	 * mm). You can use {@link #mmToPt(float)} and {@link #inToPt(float)} to
	 * convert from millimeters and inches to points.
	 * 
	 * @param width
	 *            the width, in points.
	 * @param height
	 *            the height, in points.
	 * 
	 * @see #setWidth(float)
	 * @see #setHeight(float)
	 */
	public void setSize(float width, float height) {
		setWidth(width);
		setHeight(height);
	}

	/**
	 * Sets the desired width of the "seal" image on the page.
	 * <p>
	 * If both the width and height are set to zero, the image is used in its
	 * original size. If both have positive values, the image is forced to the
	 * desired size, possibly distorting it in the process. If one has a
	 * positive value and the other is zero, the image is scaled proportionally
	 * to have the desired size in the dimension with the positive value. A
	 * negative value in either dimension is an error.
	 * <p>
	 * The sizes must be given in PostScript points (1 pt = 1/72 in = 25.4/72
	 * mm). You can use {@link #mmToPt(float)} and {@link #inToPt(float)} to
	 * convert from millimeters and inches to points.
	 * 
	 * @param width
	 *            the width, in points.
	 * 
	 * @see #setSize(float, float)
	 * @see #getWidth()
	 */
	public void setWidth(float width) {
		this.width = width;
	}

	/**
	 * Gets the desired width of the "seal" image on the page.
	 * <p>
	 * The size is returned in PostScript points (1 pt = 1/72 in = 25.4/72 mm).
	 * You can use {@link #ptToMm(float)} and {@link #ptToIn(float)} to convert
	 * from points to millimeters and inches.
	 * 
	 * @return the width, in points.
	 * 
	 * @see #setSize(float, float)
	 * @see #setWidth(float)
	 */
	public float getWidth() {
		return width;
	}

	/**
	 * Sets the desired height of the "seal" image on the page.
	 * <p>
	 * If both the width and height are set to zero, the image is used in its
	 * original size. If both have positive values, the image is forced to the
	 * desired size, possibly distorting it in the process. If one has a
	 * positive value and the other is zero, the image is scaled proportionally
	 * to have the desired size in the dimension with the positive value. A
	 * negative value in either dimension is an error.
	 * <p>
	 * The sizes must be given in PostScript points (1 pt = 1/72 in = 25.4/72
	 * mm). You can use {@link #mmToPt(float)} and {@link #inToPt(float)} to
	 * convert from millimeters and inches to points.
	 * 
	 * @param height
	 *            the height, in points.
	 * 
	 * @see #setSize(float, float)
	 * @see #getHeight()
	 */
	public void setHeight(float height) {
		this.height = height;
	}

	/**
	 * Gets the desired height of the "seal" image on the page.
	 * <p>
	 * The size is returned in PostScript points (1 pt = 1/72 in = 25.4/72 mm).
	 * You can use {@link #ptToMm(float)} and {@link #ptToIn(float)} to convert
	 * from points to millimeters and inches.
	 * 
	 * @return the height, in points.
	 * 
	 * @see #setSize(float, float)
	 * @see #setHeight(float)
	 */
	public float getHeight() {
		return height;
	}

	/**
	 * Conversion constant: the number of PostScript points in an inch.
	 */
	private static final float PT_IN_IN = 72.0F;

	/**
	 * Conversion constant: the number of PostScript points in a millimeter.
	 */
	private static final float PT_IN_MM = 72.0F / 25.4F;

	/**
	 * Converts a length from inches to PostScript points.
	 * 
	 * @param x
	 *            the length, in inches.
	 * @return the same length, in points.
	 */
	public static float inToPt(float x) {
		return x * PT_IN_IN;
	}

	/**
	 * Converts a length from PostScript points to inches.
	 * 
	 * @param x
	 *            the length, in points.
	 * @return the same length, in inches.
	 */
	public static float ptToIn(float x) {
		return x / PT_IN_IN;
	}

	/**
	 * Converts a length from millimeters to PostScript points.
	 * 
	 * @param x
	 *            the length, in millimeters.
	 * @return the same length, in points.
	 */
	public static float mmToPt(float x) {
		return x * PT_IN_MM;
	}

	/**
	 * Converts a length from PostScript points to millimeters.
	 * 
	 * @param x
	 *            the length, in points.
	 * @return the same length, in millimeters.
	 */
	public static float ptToMm(float x) {
		return x / PT_IN_MM;
	}

}
