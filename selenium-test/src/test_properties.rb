require "rubygems"

class TestProperties

  attr_accessor :selenium_url, :kontrakcja_url
  attr_accessor :browser
  attr_accessor :tester_email, :tester_password, :tester_fstname, :tester_sndname
  attr_accessor :first_counterpart_email, :first_counterpart_password, :first_counterpart_fstname, :first_counterpart_sndname
  attr_accessor :second_counterpart_email, :second_counterpart_password, :second_counterpart_fstname, :second_counterpart_sndname
  attr_accessor :third_counterpart_email, :third_counterpart_password, :third_counterpart_fstname, :third_counterpart_sndname
  attr_accessor :contract_pdf_path, :first_author_attachment_pdf_path, :second_author_attachment_pdf_path
  attr_accessor :first_sig_attachment_pdf_path, :second_sig_attachment_pdf_path
  
  def initialize
    props = {}
    
    file = File.new("selenium-test/selenium-test.properties", "r")
    while (line = file.gets)
      items = line.split()
      props[items[0]] = items[1]
    end
    
    @selenium_url = props["selenium-url"]
    @kontrakcja_url = props["kontrakcja-url"]
    @browser = props["browser"]
    @tester_email = props["tester-email"]
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
    is_missing_tester = @tester_email.nil? || @tester_password.nil?
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
    
    if is_missing_url || is_missing_tester || is_missing_a_counterpart || is_missing_pdf then
      msg = "Problem with file selenium-test/selenium-test.properties.  Expected it to contain something like:\n"
      msg += "selenium-url http://localhost:4444/wd/hub\n"
      msg += "kontrakcja-url http://localhost:8000\n"
      msg += "tester-email viktor@skrivapa.se\n"
      msg += "tester-password admin\n"
      msg += "first-counterpart-email emilymaygreen@googlemail.com\n"
      msg += "first-counterpart-password password12\n"
      msg += "first-counterpart-fstname Emily\n"
      msg += "first-counterpart-sndname Green\n"
      msg += "second-counterpart-email emilymaygreen@gmail.com\n"
      msg += "second-counterpart-password password12\n"
      msg += "second-counterpart-fstname Em\n"
      msg += "second-counterpart-sndname Green\n"
      msg += "third-counterpart-email emily@skrivapa.se\n"
      msg += "third-counterpart-password password12\n"
      msg += "third-counterpart-fstname Emily May\n"
      msg += "third-counterpart-sndname Green\n"
      raise msg
    end
  end
end
