
import java.io.FileOutputStream;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.io.File;
import java.util.ArrayList;
import java.util.Map;
import org.yaml.snakeyaml.*;
import org.yaml.snakeyaml.constructor.Constructor;

import com.itextpdf.text.DocumentException;
import com.itextpdf.text.Element;
import com.itextpdf.text.pdf.BaseFont;
import com.itextpdf.text.pdf.PdfContentByte;
import com.itextpdf.text.pdf.PdfReader;
import com.itextpdf.text.pdf.PdfStamper;
import com.itextpdf.text.pdf.PdfWriter;
import com.itextpdf.text.Rectangle;
import com.itextpdf.text.pdf.PdfSmartCopy;
import com.itextpdf.text.pdf.PdfCopy;
import com.itextpdf.text.Document;
import com.itextpdf.text.Paragraph;
import com.itextpdf.text.pdf.PdfImportedPage;
import com.itextpdf.text.Font;
import com.itextpdf.text.Font.FontFamily;
import com.itextpdf.text.BaseColor;
import com.itextpdf.text.pdf.PdfPCell;
import com.itextpdf.text.pdf.PdfPTable;
import com.itextpdf.text.Phrase;
import com.itextpdf.text.Jpeg;
import com.itextpdf.text.pdf.ColumnText;
import com.itextpdf.text.pdf.CMYKColor;
import com.itextpdf.text.pdf.events.PdfPTableEventForwarder;

class HistEntry {
    public String date;
    public String comment;
    public String address;
}

class Person {

    public String fullname;
    public String company;
    public String personalnumber;
    public String companynumber;
    public String email;
    public Boolean fullnameverified;
    public Boolean companyverified;
    public Boolean numberverified;
    public Boolean emailverified;
    public ArrayList<Field> fields;

}

class Field {
    public String valueBase64;
    public String value;
    public float x, y;
    public int page;
    public float image_w;
    public float image_h;
    public int internal_image_w;
    public int internal_image_h;
    public Boolean includeInSummary;
    public Boolean onlyForSummary;
    public float fontSize;
    public Boolean greyed;
    public ArrayList<Integer> keyColor;
}

class SealingTexts {
    public String verificationTitle;
    public String docPrefix;
    public String signedText;
    public String partnerText;
    public String secretaryText;
    public String documentText;
    public String orgNumberText;
    public String personalNumberText;
    public String eventsText;
    public String dateText;
    public String historyText;
    public String verificationFooter;
}

class SealSpec {
    public Boolean preseal;
    public String input;
    public String output;
    public String documentNumber;
    public ArrayList<Person> persons;
    public ArrayList<Person> secretaries;
    public ArrayList<HistEntry> history;
    public String initials;
    public String hostpart;
    public SealingTexts staticTexts;
    public ArrayList<SealAttachment> attachments;
    public ArrayList<FileDesc> filesList;

    public static Yaml getYaml() {
        Constructor constructor = new Constructor(SealSpec.class);

        TypeDescription sealSpecDesc = new TypeDescription(SealSpec.class);
        sealSpecDesc.putListPropertyType("persons", Person.class);
        sealSpecDesc.putListPropertyType("secretaries", Person.class);
        sealSpecDesc.putListPropertyType("history", HistEntry.class);
        sealSpecDesc.putMapPropertyType("staticTexts", String.class, String.class);
        sealSpecDesc.putListPropertyType("attachments", SealAttachment.class);
        sealSpecDesc.putListPropertyType("filesList", FileDesc.class);
        constructor.addTypeDescription(sealSpecDesc);


        TypeDescription personDesc = new TypeDescription(Person.class);
        personDesc.putListPropertyType("fields", Field.class);
        constructor.addTypeDescription(personDesc);

        Yaml yaml = new Yaml(constructor);
        return yaml;
    }

    public static SealSpec loadFromFile(String fileName) throws IOException {
        InputStream input = new FileInputStream(new File(fileName));
        Yaml yaml = getYaml();
        return (SealSpec)yaml.load(input);
    }
}

class SealAttachment {
    public String fileName;
    public String mimeType;
    public String fileBase64Content;
}

class FileDesc {
    public String title;
    public String role;
    public String pagesText;
    public String attachedBy;
}


class PdfPTableDrawFrameAroundTable extends PdfPTableEventForwarder
{
    public void tableLayout(PdfPTable table, float[][] widths, float[] heights, int headerRows, int rowStart, PdfContentByte[] canvases)
    {
        PdfContentByte lineCanvas = canvases[PdfPTable.LINECANVAS];
        Rectangle frame = new Rectangle(widths[0][0],
                                        heights[0],
                                        widths[0][widths[0].length-1],
                                        heights[heights.length-1]);
        CMYKColor frameColor = new CMYKColor(0f, 0f, 0f, 0.333f);
        frame.setBorderColor(frameColor);
        frame.setBorder(15);
        frame.setBorderWidth(1);
        lineCanvas.rectangle(frame);

        super.tableLayout(table, widths, heights, headerRows, rowStart, canvases);
    }
}

/*

Sealing works like this:

1. Open seal spec file (json/yaml) given as argument on the command line.
2. Open input file (if any).
3. On pages of input file add fields at exact positions.
4. Save it to byte stream.
5. Create final seal pages (if preseal == False).
6. Concatenate 4 and 6.
7. Save to output.

*/


public class PDFSeal {

    public static void concatenatePdfsInto(PdfReader sources[], OutputStream os)
        throws IOException, DocumentException
    {
        Document document = new Document();
        PdfCopy writer = new PdfSmartCopy(document, os);
        document.open();

        for(PdfReader reader: sources) {
            int count = reader.getNumberOfPages();
            for( int i=1; i<=count; i++ ) {
                PdfImportedPage page = writer.getImportedPage(reader, i);
                writer.addPage(page);
            }
            writer.freeReader(reader);
            reader.close();
        }

        document.close();
    }

    public static void stampFieldsOverPdf(PdfReader reader, ArrayList<Field> fields, OutputStream os)
        throws DocumentException, IOException
    {
        PdfStamper stamper = new PdfStamper(reader, os);

        PdfReader sealMarker = new PdfReader("files/sealmarker.pdf");
        PdfImportedPage sealMarkerImported = stamper.getImportedPage(sealMarker, 1);

        int count = reader.getNumberOfPages();
        for( int i=1; i<=count; i++ ) {

            Rectangle cropBox = reader.getCropBox(i);

            PdfContentByte canvas = stamper.getOverContent(i);
            for( Field field : fields ) {
                if( field.page==i &&
                    (field.onlyForSummary == null || !field.onlyForSummary)) {


                    if( field.value != null ) {

                        /*
                         * FIXME: this should somehow know the /Rotate flag.
                         */
                        float fontBaseline = 931f/(931f+225f);
                        float fontOffset   = 166f/(931f+225f);
                        float fs = field.fontSize * cropBox.getWidth();

                        if( fs<=0 )
                            fs = 10f;

                        float realx = field.x * cropBox.getWidth() + cropBox.getLeft() - fontOffset * fs;
                        float realy = (1 - field.y) * cropBox.getHeight() + cropBox.getBottom() - fontBaseline * fs;

                        BaseColor color;
                        if( !field.greyed ) {
                            color = new CMYKColor(0,0,0,1f);
                        }
                        else {
                            color = new CMYKColor(0,0,0,127);
                        }

                        Paragraph para = createParagraph(field.value, fs, Font.NORMAL, color);

                        ColumnText.showTextAligned(canvas, Element.ALIGN_LEFT,
                                                   para,
                                                   realx, realy,
                                                   0);
                    }
                    else if( field.valueBase64 !=null ) {
                        /*
                         * FIXME: this should somehow know the /Rotate flag.
                         */
                        float realx = field.x * cropBox.getWidth() + cropBox.getLeft();
                        float realy = (1 - field.y - field.image_h) * cropBox.getHeight() + cropBox.getBottom();

                        byte jpegdata[] = Base64.decode(field.valueBase64);
                        Jpeg jpeg = new Jpeg(jpegdata);

                        jpeg.setAbsolutePosition(realx, realy);
                        jpeg.scaleAbsoluteWidth(field.image_w * cropBox.getWidth());
                        jpeg.scaleAbsoluteHeight(field.image_h * cropBox.getHeight());

                        if( field.keyColor!=null ) {
                            jpeg.setTransparency(new int[] { field.keyColor.get(0),
                                                             field.keyColor.get(1),
                                                             field.keyColor.get(2)} );
                        }
                        canvas.addImage(jpeg);
                    }
                }
            }

            /*
             * Add pagination
             */
            float requestedSealSize = 18f;
            canvas.addTemplate(sealMarkerImported,
                               requestedSealSize/sealMarkerImported.getWidth(),
                               0, 0,
                               requestedSealSize/sealMarkerImported.getHeight(),
                               cropBox.getLeft() + cropBox.getWidth()/2 - requestedSealSize/2,
                               cropBox.getBottom() + 23 - requestedSealSize/2);
        }
        stamper.close();
        reader.close();
    }

    public static ArrayList<Field> getAllFields(SealSpec spec)
    {
        ArrayList<Field> result = new ArrayList<Field>();
        for( Person person : spec.persons ) {
            result.addAll(person.fields);
        }
        for( Person person : spec.secretaries ) {
            result.addAll(person.fields);
        }
        return result;
    }

    public static void addPersonsTable(Iterable<Person> persons, Document document, SealSpec spec)
        throws DocumentException, IOException
    {
        CMYKColor darkTextColor = new CMYKColor(0.806f, 0.719f, 0.51f, 0.504f);
        CMYKColor lightTextColor = new CMYKColor(0.597f, 0.512f, 0.508f, 0.201f);
        CMYKColor frameColor = new CMYKColor(0f, 0f, 0f, 0.333f);
        Paragraph para;

        PdfPTable table = new PdfPTable(2);
        table.setWidthPercentage(100f);
        table.setWidths(new int[]{1, 1});

        int cells = 0;
        for( Person person: persons ) {
            PdfPCell cell;
            cell = new PdfPCell();
            cell.setBorderColor(frameColor);
            cell.setPadding(15);
            cell.setPaddingTop(5);
            cell.setPaddingBottom(12);
            cell.setBorderWidth(1f);

            para = createParagraph(person.fullname, 10, Font.BOLD, lightTextColor );
            para.setLeading(0f, 1.2f);
            cell.addElement(para);
            para = createParagraph(person.company, 10, Font.NORMAL, lightTextColor);
            para.setLeading(0f, 1.2f);
            para.setSpacingAfter(13);
            cell.addElement(para);
            if( person.personalnumber!=null && person.personalnumber!="" ) {
                para = createParagraph(spec.staticTexts.personalNumberText + " " + person.personalnumber, 10, Font.NORMAL, lightTextColor );
                para.setLeading(0f, 1.2f);
                cell.addElement(para);
            }
            if( person.companynumber!=null && person.companynumber!="" ) {
                para = createParagraph(spec.staticTexts.orgNumberText + " " + person.companynumber, 10, Font.NORMAL, lightTextColor);
                para.setLeading(0f, 1.2f);
                cell.addElement(para);
            }
            for( Field field : person.fields ) {
                if( field.valueBase64!=null &&
                    field.valueBase64!="" &&
                    field.includeInSummary ) {
                        byte jpegdata[] = Base64.decode(field.valueBase64);
                        Jpeg jpeg = new Jpeg(jpegdata);

                        if( field.keyColor!=null ) {
                            jpeg.setTransparency(new int[] { field.keyColor.get(0),
                                                             field.keyColor.get(1),
                                                             field.keyColor.get(2)} );
                        }
                        jpeg.scaleAbsoluteWidth(150);
                        jpeg.setBorder(Rectangle.BOTTOM);
                        jpeg.setBorderWidth(1f);
                        jpeg.setBorderColor(lightTextColor);
                        cell.addElement(jpeg);
                }
            }
            table.addCell(cell);
            cells++;
        }
        if( (cells&1)!=0 ) {
            PdfPCell cell = new PdfPCell();
            cell.setBorder(PdfPCell.NO_BORDER);
            table.addCell(cell);
        }

        document.add(table);
    }

    public static void addSubtitle(Document document, String text)
        throws DocumentException, IOException
    {
        CMYKColor darkTextColor = new CMYKColor(0.806f, 0.719f, 0.51f, 0.504f);
        Paragraph para = createParagraph(text, 12, Font.NORMAL, darkTextColor);
        para.setFirstLineIndent(7f);
        para.setSpacingBefore(10);
        para.setSpacingAfter(10);
        document.add(para);
    }

    public static void prepareSealPages(SealSpec spec, OutputStream os)
        throws IOException, DocumentException
    {
        Document document = new Document();
        PdfWriter writer = PdfWriter.getInstance(document, os);

        document.open();

        CMYKColor darkTextColor = new CMYKColor(0.806f, 0.719f, 0.51f, 0.504f);
        CMYKColor lightTextColor = new CMYKColor(0.597f, 0.512f, 0.508f, 0.201f);
        CMYKColor frameColor = new CMYKColor(0f, 0f, 0f, 0.333f);


        PdfPTableDrawFrameAroundTable drawFrame = new PdfPTableDrawFrameAroundTable();

        /*
         * FIXME: To use chineese characters you need to define BaseFont, like this:
         *
         * BaseFont bf = BaseFont.createFont("STSong-Light", "UniGB-UCS2-H", BaseFont.EMBEDDED);
         *
         */

        Font font;

        document.setMargins(document.leftMargin(),
                            document.rightMargin(),
                            document.topMargin(),
                            document.bottomMargin() + 130);

        document.newPage();

        document.add(createParagraph(spec.staticTexts.verificationTitle, 21, Font.NORMAL, darkTextColor));

        Paragraph para = createParagraph(spec.staticTexts.docPrefix + " " + spec.documentNumber, 12, Font.NORMAL, lightTextColor);
        para.setSpacingAfter(50);
        document.add(para);

        /*
         * Warning for future generations:
         *
         * itext will not show row of a table that is not full of
         * cells. You have to add one last empty cell to get it going.
         */

        /*
         * Document part
         */
        addSubtitle(document, spec.staticTexts.documentText);

        PdfPTable table = new PdfPTable(2);
        int cells;
        table.setWidthPercentage(100f);
        table.setWidths(new int[]{1, 1});

        cells = 0;
        for( FileDesc file : spec.filesList ) {
            PdfPCell cell;
            cell = new PdfPCell();
            cell.setBorderColor(frameColor);
            cell.setPadding(15);
            cell.setPaddingTop(5);
            cell.setPaddingBottom(12);
            cell.setBorderWidth(1f);

            para = createParagraph(file.title, 10, Font.BOLD, lightTextColor);
            para.setLeading(0f, 1.2f);
            cell.addElement(para);
            para = createParagraph(file.role, 10, Font.NORMAL, lightTextColor);
            para.setLeading(0f, 1.2f);
            cell.addElement(para);
            para = createParagraph(file.pagesText, 10, Font.NORMAL, lightTextColor);
            para.setLeading(0f, 1.2f);
            cell.addElement(para);
            para = createParagraph(file.attachedBy, 10, Font.ITALIC, lightTextColor);
            para.setLeading(0f, 1.2f);
            cell.addElement(para);
            table.addCell(cell);
            cells++;
        }
        if( (cells&1)!=0 ) {
            PdfPCell cell = new PdfPCell();
            cell.setBorder(PdfPCell.NO_BORDER);
            table.addCell(cell);
        }
        document.add(table);

        /*
         * Partners part
         */
        addSubtitle(document, spec.staticTexts.partnerText);
        addPersonsTable(spec.persons, document, spec);

        /*
         * Secretaries part
         */
        if( spec.secretaries!=null && !spec.secretaries.isEmpty()) {
            addSubtitle(document, spec.staticTexts.secretaryText);
            addPersonsTable(spec.secretaries, document, spec);
        }

        addSubtitle(document, spec.staticTexts.eventsText);

        table = new PdfPTable(2);
        table.setWidthPercentage(100f);
        table.setWidths(new int[]{1, 2});

        table.setTableEvent(drawFrame);

        for( HistEntry entry: spec.history ) {

            PdfPCell cell;
            cell = new PdfPCell();
            cell.setBorder(0);
            cell.setPaddingLeft(15);
            cell.setPaddingRight(15);

            cell.addElement(createParagraph(entry.date, 10, Font.ITALIC, lightTextColor));
            cell.addElement(createParagraph(entry.address, 8, Font.ITALIC, lightTextColor));
            table.addCell(cell);

            cell = new PdfPCell();
            cell.setBorder(0);
            cell.setPaddingLeft(15);
            cell.setPaddingRight(15);
            para = createParagraph(entry.comment, 10, Font.ITALIC, lightTextColor);
            para.setLeading(0f, 1.2f);
            para.setSpacingAfter(7);
            cell.addElement(para);
            table.addCell(cell);
        }
        document.add(table);

        document.close();
    }

    public static void stampFooterOverSealPages(SealSpec spec, PdfReader reader, ByteArrayOutputStream sealPages)
        throws IOException, DocumentException {
        CMYKColor frameColor = new CMYKColor(0f, 0f, 0f, 0.333f);
        CMYKColor lightTextColor = new CMYKColor(0.597f, 0.512f, 0.508f, 0.201f);
        PdfStamper stamper = new PdfStamper(reader, sealPages);
        PdfReader sealMarker = new PdfReader("files/sealmarker.pdf");
        Rectangle pageFrame = new Rectangle(581.839f-567.36f, 14.37f, 581.839f, 813.12f + 14.37f);
        pageFrame.setBorderColor(frameColor);
        pageFrame.setBorderWidth(1);
        pageFrame.setBorder(15);


        Document document = new Document();
        document.open();

        float printableWidth = 567f;
        float printableMargin = 23f;
        Rectangle pageSize = document.getPageSize();

        Rectangle footerFrame = new Rectangle(document.leftMargin(), document.bottomMargin(),
                                              pageSize.getWidth() - document.rightMargin(), document.bottomMargin() + 80);
        footerFrame.setBorderColor(frameColor);
        footerFrame.setBorderWidth(1);
        footerFrame.setBorder(15);


        PdfImportedPage sealMarkerImported = stamper.getImportedPage(sealMarker, 1);

        Rectangle footerTextRect = new Rectangle(footerFrame.getLeft() + 15,
                                                 footerFrame.getBottom() + 10,
                                                 footerFrame.getRight() - sealMarkerImported.getWidth() - 5 - 15,
                                                 footerFrame.getTop() - 10);

        int pageCount = reader.getNumberOfPages();
        for( int i=1; i<=pageCount; i++ ) {
            PdfContentByte canvasUnder = stamper.getUnderContent(i);

            canvasUnder.rectangle(pageFrame);

            PdfContentByte canvasOver = stamper.getOverContent(i);

            canvasOver.rectangle(footerFrame);

            canvasOver.addTemplate(sealMarkerImported, footerFrame.getRight() - sealMarkerImported.getWidth() - 5,
                                   footerFrame.getBottom() +
                                   (footerFrame.getHeight() - sealMarkerImported.getHeight())/2);

            ColumnText columnText = new ColumnText(canvasOver);

            columnText.setSimpleColumn(footerTextRect);
            columnText.setLeading(0f, 1.2f);

            Paragraph para = createParagraph(spec.staticTexts.verificationFooter, 8, Font.NORMAL, lightTextColor);
            para.setLeading(0f, 1.2f);
            columnText.addElement(para);

            para = createParagraph(i + "/" + pageCount, 8, Font.NORMAL, lightTextColor);
            para.setLeading(3f, 1.2f);
            columnText.addElement(para);
            columnText.go();
        }
        stamper.close();
        reader.close();
    }

    /**
     * Manipulates a PDF file src with the file dest as result
     * @param src the original PDF
     * @param dest the resulting PDF
     * @throws IOException
     * @throws DocumentException
     */
    public static void manipulatePdf(SealSpec spec) throws IOException, DocumentException {

        ByteArrayOutputStream sealPagesRaw = new ByteArrayOutputStream();
        ByteArrayOutputStream sealPages = new ByteArrayOutputStream();
        ByteArrayOutputStream sourceWithFields = new ByteArrayOutputStream();

        prepareSealPages(spec, sealPagesRaw);

        stampFooterOverSealPages(spec, new PdfReader(sealPagesRaw.toByteArray()), sealPages);

        stampFieldsOverPdf(new PdfReader(spec.input), getAllFields(spec), sourceWithFields);

        concatenatePdfsInto(new PdfReader[] { new PdfReader(sourceWithFields.toByteArray()),
                                              new PdfReader(sealPages.toByteArray()) },
                            new FileOutputStream(spec.output));
    }


    static BaseFont baseFontHelvetica;
    static Font getFontForString(String text, float size, int style, BaseColor color)
        throws DocumentException, IOException
    {
        if(baseFontHelvetica==null ) {
            baseFontHelvetica = BaseFont.createFont("Helvetica.ttf",  BaseFont.IDENTITY_H, BaseFont.EMBEDDED);
            baseFontHelvetica.setSubset(true);
        }
        return new Font(baseFontHelvetica, size, style, color);
    }

    static Paragraph createParagraph(String text, float size, int style, BaseColor color)
        throws DocumentException, IOException
    {
        Font font = getFontForString(text, size, style, color);
        return new Paragraph(text, font);
    }

    /**
     * Main method.
     *
     * @param    args    no arguments needed
     * @throws DocumentException
     * @throws IOException
     */
    public static void main(String[] args) throws IOException, DocumentException {
        DumperOptions options = new DumperOptions();
        options.setDefaultFlowStyle(DumperOptions.FlowStyle.BLOCK);
        //options.setDefaultFlowStyle(DumperOptions.FlowStyle.FLOW );
        //options.setDefaultScalarStyle(DumperOptions.ScalarStyle.DOUBLE_QUOTED );
        Yaml yaml = new Yaml(options);
        SealSpec spec = SealSpec.loadFromFile(args[0]);

        System.out.println(yaml.dump(spec));

        manipulatePdf(spec);
    }
}
