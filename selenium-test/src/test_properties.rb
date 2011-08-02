require "rubygems"

class TestProperties

  attr_accessor :selenium_url, :kontrakcja_url
  attr_accessor :tester_email, :tester_password
  attr_accessor :first_counterpart_email, :first_counterpart_password, :first_counterpart_fstname, :first_counterpart_sndname
  attr_accessor :contract_pdf_path
  
  def initialize
    props = {}
    
    file = File.new("selenium-test/selenium-test.properties", "r")
    while (line = file.gets)
      items = line.split()
      props[items[0]] = items[1]
    end
    
    @selenium_url = props["selenium-url"]
    @kontrakcja_url = props["kontrakcja-url"]
    @tester_email = props["tester-email"]
    @tester_password = props["tester-password"]
    @first_counterpart_email = props["first-counterpart-email"]
    @first_counterpart_password = props["first-counterpart-password"]
    @first_counterpart_fstname = props["first-counterpart-fstname"]
    @first_counterpart_sndname = props["first-counterpart-sndname"]
    @contract_pdf_path = props["contract-pdf-path"]
    
    is_missing_url = @selenium_url.nil? || @kontrakcja_url.nil?
    is_missing_tester = @tester_email.nil? || @tester_password.nil?
    is_missing_first_counterpart = @first_counterpart_email.nil? || @first_counterpart_password.nil? || @first_counterpart_fstname.nil? || @first_counterpart_sndname.nil?
    is_missing_pdf = @contract_pdf_path.nil?
    
    if is_missing_url || is_missing_tester || is_missing_first_counterpart || is_missing_pdf then
      msg = "Problem with file selenium-test/selenium-test.properties.  Expected it to contain something like:\n"
      msg += "selenium-url http://localhost:4444/wd/hub\n"
      msg += "kontrakcja-url http://localhost:8000\n"
      msg += "tester-email viktor@skrivapa.se\n"
      msg += "tester-password admin\n"
      msg += "first-counterpart-email emilymaygreen@googlemail.com\n"
      msg += "first-counterpart-password password12\n"
      msg += "first-counterpart-fstname Emily\n"
      msg += "first-counterpart-sndname Green\n"
      msg += "contract-pdf-path ~/samples/contract.pdf"
      raise msg
    end
  end
end
