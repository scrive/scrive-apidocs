require "rubygems"
gem "rspec"
gem "selenium-client"
require "selenium/client"
require "selenium/rspec/spec_helper"
require "spec/test/unit"
require "src/user_helper"
include UserHelper
require "src/upload_helper"
include UploadHelper
require "src/footer_helper"
include FooterHelper

describe "activation page" do
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

  before(:each) do
    page.open "/signup"
    page.wait_for_page_to_load "30000"
  end
  
  append_after(:each) do
    @verification_errors.should == []
  end

  append_after(:all) do
    @selenium_driver.close_current_browser_session
  end

  it "has login form in header when nobody is logged in" do
    UserHelper.is_login_form_present_in_header(page).should be_true
  end

  it "has logout link in header when somebody is logged in" do
    UserHelper.login_as_new_user(page, "gertie@corp.xyz", "Gertie Gray", "passwordg")
    begin
      UserHelper.is_logout_link_present_in_header(page).should be_true
    ensure
      UserHelper.logout(page)
    end
  end
  
  it "displays the tos until activation complete" do
    UserHelper.register(page, "harry@comltd.xyz")

    UserHelper.follow_activation_link(page, "harry@comltd.xyz")
    UserHelper.is_tos_present(page).should be_true

    UserHelper.tick_tos(page)

    UserHelper.follow_activation_link(page, "harry@comltd.xyz")
    UserHelper.is_tos_present(page).should be_true

    UserHelper.activate(page, "harry@comltd.xyz", "Harry Happstack", "passwordh", "passwordh")
    begin
      UserHelper.follow_activation_link(page, "harry@comltd.xyz")
      UserHelper.is_tos_present(page).should be_false 
    ensure
      UserHelper.logout(page)
    end   
 
    UserHelper.follow_activation_link(page, "harry@comltd.xyz")
    UserHelper.is_tos_present(page).should be_false 
  end

  it "logs the user in and displays the upload form once activation is complete" do
    UserHelper.register(page, "irene@comltd.xyz")
    UserHelper.activate(page, "irene@comltd.xyz", "Irene Iris", "passwordi", "passwordi")
    begin
      UserHelper.is_logout_link_present_in_header(page).should be_true
      UploadHelper.is_upload_form_present(page).should be_true
    ensure
      UserHelper.logout(page)
    end
  end

  it "displays the activation form again if passwords don't match" do
    UserHelper.register(page, "jack@comltd.xyz")
    UserHelper.activate(page, "jack@comltd.xyz", "Jack Jones", "passwordj", "not_passwordj")
    UserHelper.is_activation_form_present(page).should be_true
  end

  it "allows user to activate with a blank name" do
    UserHelper.register(page, "kat@comltd.xyz")
    UserHelper.activate(page, "kat@comltd.xyz", "", "passwordk", "passwordk")
    begin
      UserHelper.is_logout_link_present_in_header(page).should be_true
    ensure
      UserHelper.logout(page)
    end
  end

  it "displays activation form again if passwords are left empty" do
    UserHelper.register(page, "liam@comltd.xyz")
    UserHelper.activate(page, "liam@comltd.xyz", "Liam Loh", "", "")
    UserHelper.is_activation_form_present(page).should be_true
  end

  it "displays activation form again if passwords don't match" do
    UserHelper.register(page, "marjorie@comltd.xyz")
    UserHelper.activate(page, "marjorie@comltd.xyz", "Mo Mowlem", "passwordm", "not_passwordm")
    UserHelper.is_activation_form_present(page).should be_true
  end

  it "displays activation form again if password shorter than 6 chars" do
    UserHelper.register(page, "nigel@comltd.xyz")
    UserHelper.activate(page, "nigel@comltd.xyz", "Nigel Noodel", "just5", "just5")
    UserHelper.is_activation_form_present(page).should be_true
  end

  it "has all footer links" do
    FooterHelper.are_footer_links_present(page).should be_true
  end
end
