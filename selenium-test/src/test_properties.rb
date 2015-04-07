require "rubygems"
require "json"

class TestProperties

  attr_accessor :selenium_url, :kontrakcja_url
  attr_accessor :browser
  attr_accessor :admin_email, :admin_password
  attr_accessor :tester_emails, :tester_password, :tester_fstname, :tester_sndname
  attr_accessor :first_counterpart_email, :first_counterpart_password, :first_counterpart_fstname, :first_counterpart_sndname
  attr_accessor :second_counterpart_email, :second_counterpart_password, :second_counterpart_fstname, :second_counterpart_sndname
  attr_accessor :third_counterpart_email, :third_counterpart_password, :third_counterpart_fstname, :third_counterpart_sndname
  attr_accessor :contract_pdf_path, :first_author_attachment_pdf_path, :second_author_attachment_pdf_path
  attr_accessor :first_sig_attachment_pdf_path, :second_sig_attachment_pdf_path

  def tester_email
    lang = ENV['SELENIUM_TEST_LANG']
    return @tester_emails[lang]
  end
  
  def initialize
    file = File.read("selenium-test/selenium-test.properties")
    props = JSON.parse(file)

    @selenium_url = props["selenium-url"]
    @kontrakcja_url = props["kontrakcja-url"]
    @browser = props["browser"]
    @admin_email = props["admin-email"]
    @admin_password = props["admin-password"]
    @tester_emails = props["tester-emails"]
    @tester_password = props["tester-password"]
    @tester_fstname = props["tester-fstname"]
    @tester_sndname = props["tester-sndname"]
    @first_counterpart_email = props["first-counterpart-email"]
    @first_counterpart_password = props["first-counterpart-password"]
    @first_counterpart_fstname = props["first-counterpart-fstname"]
    @first_counterpart_sndname = props["first-counterpart-sndname"]
    @second_counterpart_email = props["second-counterpart-email"]
    @second_counterpart_password = props["second-counterpart-password"]
    @second_counterpart_fstname = props["second-counterpart-fstname"]
    @second_counterpart_sndname = props["second-counterpart-sndname"]
    @third_counterpart_email = props["third-counterpart-email"]
    @third_counterpart_password = props["third-counterpart-password"]
    @third_counterpart_fstname = props["third-counterpart-fstname"]
    @third_counterpart_sndname = props["third-counterpart-sndname"]
    @contract_pdf_path = props["contract-pdf-path"]
    @first_author_attachment_pdf_path = props["first-author-attachment-pdf-path"]
    @second_author_attachment_pdf_path = props["second-author-attachment-pdf-path"]
    @first_sig_attachment_pdf_path = props["first-sig-attachment-pdf-path"]
    @second_sig_attachment_pdf_path = props["second-sig-attachment-pdf-path"]
    
    
    is_missing_url = @kontrakcja_url.nil?
    is_missing_admin = @admin_email.nil? || @admin_password.nil?
    is_missing_tester = tester_email.nil? || @tester_password.nil?
    is_missing_first_counterpart = @first_counterpart_email.nil? || @first_counterpart_password.nil? || @first_counterpart_fstname.nil? || @first_counterpart_sndname.nil?
    is_missing_second_counterpart = @second_counterpart_email.nil? || @second_counterpart_password.nil? || @second_counterpart_fstname.nil? || @second_counterpart_sndname.nil?
    is_missing_third_counterpart = @third_counterpart_email.nil? || @third_counterpart_password.nil? || @third_counterpart_fstname.nil? || @third_counterpart_sndname.nil?
    is_missing_a_counterpart = @is_missing_first_counterpart || @is_missing_second_counterpart || @is_missing_third_counterpart
    if @contract_pdf_path.nil? then
      @contract_pdf_path = Dir.pwd + "/selenium-test/samples/contract.pdf"
    end
    if @first_author_attachment_pdf_path.nil? then
      @first_author_attachment_pdf_path = Dir.pwd + "/selenium-test/samples/author-att-1.pdf"
    end
    if @second_author_attachment_pdf_path.nil? then
      @second_author_attachment_pdf_path = Dir.pwd + "/selenium-test/samples/author-att-2.pdf"
    end
    if @first_sig_attachment_pdf_path.nil? then
      @first_sig_attachment_pdf_path = Dir.pwd + "/selenium-test/samples/sig-att-1.pdf"
    end
    if @second_sig_attachment_pdf_path.nil? then
      @second_sig_attachment_pdf_path = Dir.pwd + "/selenium-test/samples/sig-att-2.pdf"
    end
    is_missing_pdf = @contract_pdf_path.nil? || @first_author_attachment_pdf_path.nil? || @second_author_attachment_pdf_path.nil? || @first_sig_attachment_pdf_path.nil? || @second_sig_attachment_pdf_path.nil?
    
    if is_missing_url || is_missing_admin || is_missing_tester || is_missing_a_counterpart || is_missing_pdf then
      msg = "Problem with file selenium-test/selenium-test.properties.  Expected it to contain something like:\n"
      msg += "{\"kontrakcja-url\": \"http://localhost:8000\",\n"
      msg += " \"browser\": \"firefox\",\n"
      msg += " \"admin-email\": \"admin@example.com\",\n"
      msg += " \"admin-password\": \"admin\",\n"
      msg += " \"tester-emails\": {\"en\": \"selenium-en@skrivapa.se\",\n"
      msg += "                     \"sv\": \"selenium-sv@skrivapa.se\",\n"
      msg += "                     ...\n"
      msg += "                     \"fi\": \"selenium-fi@skrivapa.se\"}\n"
      msg += " \"tester-password\": \"admin\",\n"
      msg += " \"tester-fstname\": \"James\",\n"
      msg += " \"tester-sndname\": \"Bond\",\n"
      msg += " \"first-counterpart-password\": \"password12\",\n"
      msg += " \"first-counterpart-fstname\": \"Bob\",\n"
      msg += " \"first-counterpart-sndname\": \"Blue\",\n"
      msg += " \"second-counterpart-password\": \"password12\",\n"
      msg += " \"second-counterpart-fstname\": \"Fred\",\n"
      msg += " \"second-counterpart-sndname\": \"Frog\",\n"
      msg += " \"third-counterpart-password\": \"password12\",\n"
      msg += " \"third-counterpart-fstname\": \"Gordon\",\n"
      msg += " \"third-counterpart-sndname\": \"Gecko\",\n"
      raise msg
    end
  end
end
