
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
import com.itextpdf.text.BaseColor;
import com.itextpdf.text.Rectangle;
import com.itextpdf.text.pdf.PdfPCell;
import com.itextpdf.text.pdf.PdfPTable;
import com.itextpdf.text.Phrase;


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
    public Double x, y;
    public int page;
    public double image_w;
    public double image_h;
    public int internal_image_w;
    public int internal_image_h;
    public Boolean includeInSummary;
    public Boolean onlyForSummary;
    public double fontSize;
    public Boolean greyed;
    public ArrayList<Integer> keyColor;
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
    public Map<String,String> staticTexts;
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

    /** The resulting PDF. */
    public static final String RESULT = "appended.pdf";
    public static final String INPUT  = "input.pdf";

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

    public void prepareSealPages(OutputStream os)
        throws IOException, DocumentException
    {
        Document document = new Document();
        PdfWriter writer = PdfWriter.getInstance(document, os);

        document.open();

        BaseFont bf;
        Font font;
        bf = BaseFont.createFont("STSong-Light", "UniGB-UCS2-H", BaseFont.EMBEDDED);

        font = new Font(bf, 12);

        // step 4
        document.newPage();
        document.add(new Paragraph("This page will NOT be followed by a blank page! \u5341\u950a\u57cb\u4f0f", font));
        document.newPage();
        // we don't add anything to this page: newPage() will be ignored
        document.newPage();
        document.add(new Paragraph("This page will be followed by a blank page!"));
        document.newPage();
        writer.setPageEmpty(false);
        document.newPage();
        document.add(new Paragraph("The previous page was a blank page!"));


        PdfPTable table = new PdfPTable(3);
        table.setWidthPercentage(288 / 5.23f);
        table.setWidths(new int[]{2, 1, 1});
        PdfPCell cell;
        cell = new PdfPCell(new Phrase("Table 1"));
        cell.setColspan(3);
        table.addCell(cell);
        cell = new PdfPCell(new Phrase("Cell with rowspan 2"));
        cell.setRowspan(2);
        cell.setBorder(Rectangle.LEFT | Rectangle.TOP | Rectangle.RIGHT);
        cell.setBorderColor(BaseColor.CYAN);
        cell.setBorderWidth(5);

        table.addCell(cell);
        table.addCell("row 1; cell 1");
        table.addCell("row 1; cell 2");
        table.addCell("row 2; cell 1");
        table.addCell("row 2; cell 2");
        document.add(table);

        // step 5
        document.close();
    }

    /**
     * Manipulates a PDF file src with the file dest as result
     * @param src the original PDF
     * @param dest the resulting PDF
     * @throws IOException
     * @throws DocumentException
     */
    public void manipulatePdf(String src, String dest) throws IOException, DocumentException {

        ByteArrayOutputStream os = new ByteArrayOutputStream();

        prepareSealPages(os);

        concatenatePdfsInto(new PdfReader[] { new PdfReader(src), new PdfReader(os.toByteArray()) },
                            new FileOutputStream(dest));
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
        SealSpec spec = SealSpec.loadFromFile("example_spec.json");

        /*
        SealSpec spec = new SealSpec();
        spec.input = "input.pdf";
        spec.output = "output.pdf";
        spec.documentNumber = "123114234124";
        spec.persons = new ArrayList<Person>();
        spec.filesList = new ArrayList<FileDesc>();
        FileDesc fileDesc = new FileDesc();
        fileDesc.fileTitle = "  File title  ";
        fileDesc.fileRole = "File role";
        fileDesc.filePagesText = "null";
        fileDesc.fileAttachedBy = "File attached by";
        spec.filesList.add(fileDesc);
        */
        System.out.println(yaml.dump(spec));

        new AppendMode().manipulatePdf(INPUT, RESULT);
    }
}
