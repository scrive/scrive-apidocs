
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
import com.itextpdf.text.Rectangle;
import com.itextpdf.text.pdf.PdfPCell;
import com.itextpdf.text.pdf.PdfPTable;
import com.itextpdf.text.Phrase;
import com.itextpdf.text.Jpeg;
import com.itextpdf.text.pdf.ColumnText;
import com.itextpdf.text.pdf.CMYKColor;

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
    public double fontSize;
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

        int count = reader.getNumberOfPages();
        for( int i=1; i<=count; i++ ) {

            Rectangle cropBox = reader.getCropBox(i);

            PdfContentByte canvas = stamper.getOverContent(i);
            for( Field field : fields ) {
                if( field.page==i &&
                    (field.onlyForSummary == null || !field.onlyForSummary)) {

                    /*
                     * FIXME: this should somehow know the /Rotate flag.
                     */
                    float realx = field.x * cropBox.getWidth() + cropBox.getLeft();
                    float realy = field.y * cropBox.getHeight() + cropBox.getBottom();

                    if( field.value != null ) {
                        System.out.println("Placing " + field.value + " at " + realx + "," + realy);

                        Font font = new Font(FontFamily.HELVETICA, (float)(cropBox.getWidth() * field.fontSize));

                        if( field.greyed ) {
                            font.setColor( new CMYKColor(0,0,0,127));
                        }

                        ColumnText.showTextAligned(canvas, Element.ALIGN_LEFT,
                                                   new Phrase(field.value, font),
                                                   realx, realy,
                                                   0);
                    }
                    else if( field.valueBase64 !=null ) {
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

        PdfPTable table = new PdfPTable(2);
        table.setWidthPercentage(100f);
        table.setWidths(new int[]{1, 1});

        int cells = 0;
        for( Person person: persons ) {
            PdfPCell cell;
            cell = new PdfPCell();
            cell.setBorderColor(lightTextColor);

            Font font = new Font(FontFamily.HELVETICA, 10, Font.BOLD, darkTextColor );
            cell.addElement(new Paragraph(person.fullname, font));
            font = new Font(FontFamily.HELVETICA, 10, Font.NORMAL, darkTextColor );
            cell.addElement(new Paragraph(person.company, font));
            if( person.personalnumber!=null && person.personalnumber!="" ) {
                font = new Font(FontFamily.HELVETICA, 10, Font.NORMAL, darkTextColor );
                cell.addElement(new Paragraph(spec.staticTexts.personalNumberText + " " + person.personalnumber, font));
            }
            if( person.companynumber!=null && person.companynumber!="" ) {
                font = new Font(FontFamily.HELVETICA, 10, Font.NORMAL, darkTextColor );
                cell.addElement(new Paragraph(spec.staticTexts.orgNumberText + " " + person.companynumber, font));
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

    public static void prepareSealPages(SealSpec spec, OutputStream os)
        throws IOException, DocumentException
    {
        Document document = new Document();
        PdfWriter writer = PdfWriter.getInstance(document, os);

        document.open();

        CMYKColor darkTextColor = new CMYKColor(0.806f, 0.719f, 0.51f, 0.504f);
        CMYKColor lightTextColor = new CMYKColor(0.597f, 0.512f, 0.508f, 0.201f);

        /*
         * FIXME: To use chineese characters you need to define BaseFont, like this:
         *
         * BaseFont bf = BaseFont.createFont("STSong-Light", "UniGB-UCS2-H", BaseFont.EMBEDDED);
         *
         */

        Font font;

        document.newPage();

        font = new Font(FontFamily.HELVETICA, 21, Font.NORMAL, darkTextColor );
        document.add(new Paragraph(spec.staticTexts.verificationTitle, font));

        font = new Font(FontFamily.HELVETICA, 12, Font.NORMAL, lightTextColor );
        document.add(new Paragraph(spec.staticTexts.docPrefix + " " + spec.documentNumber, font));

        /*
         * Warning for future generations:
         *
         * itext will not show row of a table that is not full of
         * cells. You have to add one last empty cell to get it going.
         */

        /*
         * Document part
         */
        font = new Font(FontFamily.HELVETICA, 12, Font.NORMAL, darkTextColor );
        document.add(new Paragraph(spec.staticTexts.documentText, font));

        PdfPTable table = new PdfPTable(2);
        int cells;
        table.setWidthPercentage(100f);
        table.setWidths(new int[]{1, 1});

        cells = 0;
        for( FileDesc file : spec.filesList ) {
            PdfPCell cell;
            cell = new PdfPCell();
            cell.setBorderColor(lightTextColor);

            font = new Font(FontFamily.HELVETICA, 12, Font.BOLD, darkTextColor );
            cell.addElement(new Paragraph(file.title, font));
            font = new Font(FontFamily.HELVETICA, 12, Font.NORMAL, darkTextColor );
            cell.addElement(new Paragraph(file.role, font));
            font = new Font(FontFamily.HELVETICA, 12, Font.NORMAL, darkTextColor );
            cell.addElement(new Paragraph(file.pagesText, font));
            font = new Font(FontFamily.HELVETICA, 12, Font.ITALIC, darkTextColor );
            cell.addElement(new Paragraph(file.attachedBy, font));
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
        font = new Font(FontFamily.HELVETICA, 12, Font.NORMAL, darkTextColor );
        document.add(new Paragraph(spec.staticTexts.partnerText, font));
        addPersonsTable(spec.persons, document, spec);

        /*
         * Secretaries part
         */
        if( spec.secretaries!=null && !spec.secretaries.isEmpty()) {
            font = new Font(FontFamily.HELVETICA, 12, Font.NORMAL, darkTextColor );
            document.add(new Paragraph(spec.staticTexts.secretaryText, font));
            addPersonsTable(spec.secretaries, document, spec);
        }

        font = new Font(FontFamily.HELVETICA, 12, Font.NORMAL, darkTextColor );
        document.add(new Paragraph(spec.staticTexts.eventsText, font));

        table = new PdfPTable(2);
        table.setWidthPercentage(100f);
        table.setWidths(new int[]{1, 2});

        for( HistEntry entry: spec.history ) {

            PdfPCell cell;
            cell = new PdfPCell();
            cell.setBorderColor(lightTextColor);

            font = new Font(FontFamily.HELVETICA, 10, Font.ITALIC, lightTextColor );
            cell.addElement(new Paragraph(entry.date, font));
            font = new Font(FontFamily.HELVETICA, 8, Font.ITALIC, lightTextColor );
            cell.addElement(new Paragraph(entry.address, font));
            table.addCell(cell);

            cell = new PdfPCell();
            cell.setBorderColor(lightTextColor);
            font = new Font(FontFamily.HELVETICA, 10, Font.ITALIC, lightTextColor );
            cell.addElement(new Paragraph(entry.comment, font));
            table.addCell(cell);
        }
        document.add(table);

        document.close();
    }

    /**
     * Manipulates a PDF file src with the file dest as result
     * @param src the original PDF
     * @param dest the resulting PDF
     * @throws IOException
     * @throws DocumentException
     */
    public static void manipulatePdf(SealSpec spec) throws IOException, DocumentException {

        ByteArrayOutputStream os = new ByteArrayOutputStream();
        prepareSealPages(spec, os);

        ByteArrayOutputStream os2 = new ByteArrayOutputStream();
        stampFieldsOverPdf(new PdfReader(spec.input), getAllFields(spec), os2);

        concatenatePdfsInto(new PdfReader[] { new PdfReader(os2.toByteArray()), new PdfReader(os.toByteArray()) },
                            new FileOutputStream(spec.output));
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
