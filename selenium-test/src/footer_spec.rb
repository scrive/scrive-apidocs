require "rubygems"
gem "rspec"
gem "selenium-client"
require "selenium/client"
require "selenium/rspec/spec_helper"
require "spec/test/unit"
require "src/user_helper"
include UserHelper
require "src/footer_helper"
include FooterHelper

describe "footer" do
  attr_reader :selenium_driver
  alias :page :selenium_driver

  before(:all) do
    @verification_errors = []
    @selenium_driver = Selenium::Client::Driver.new \
      :host => "localhost",
      :port => 4444,
      :browser => "*chrome",
      :url => "http://localhost:8000/",
      :timeout_in_second => 60
    @selenium_driver.start_new_browser_session
  end
  
  append_after(:each) do
    @verification_errors.should == []
  end

  append_after(:all) do
    @selenium_driver.close_current_browser_session
  end

  def checkExistsInTitle(staticPage, title)
    page.open staticPage
    page.wait_for_page_to_load "30000"
    page.is_text_present(title).should be_true
    page.get_text("//h1").include?title.should be_true
  end

  it "has a static why page containing header text 'Varför'" do
    checkExistsInTitle("/why.html", "Varför")
  end

  it "has a static features page containing header text 'Funktioner'" do
    checkExistsInTitle("/features.html", "Funktioner")
  end

  it "has a static pricing page containing header text 'Prisplan'" do
    checkExistsInTitle("/pricing.html", "Prisplan")
  end

  it "has a static security page containg text 'Säkerhet'" do
    checkExistsInTitle("/security.html", "Säkerhet")
  end

  it "has a static legal page containing header text 'Juridik'" do
    checkExistsInTitle("/legal.html", "Juridik")
  end

  it "has a static privacy policy page containing header text 'Sekretesspolicy'" do
    checkExistsInTitle("/privacypolicy.html", "Sekretesspolicy")
  end

  it "has a static terms of use page containing header text 'VILLKOR'" do
    checkExistsInTitle("/termsofuse.html", "VILLKOR")
  end

  it "has a static contact page containing header text 'Kontakt'" do
    checkExistsInTitle("/contact.html", "Kontakt")
  end

end
