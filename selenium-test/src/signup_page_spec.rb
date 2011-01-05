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

describe "signup page" do
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
    UserHelper.register_login(page, "Harry Happstack", "harry@corp.xyz", "passwordh")
    begin
      UserHelper.is_logout_link_present_in_header(page).should be_true
    ensure
      UserHelper.logout(page)
    end
  end
  
  it "has all footer links" do
    FooterHelper.are_footer_links_present(page).should be_true
  end

  it "displays signup form again if passwords don't match" do
    UserHelper.register_with_second_password(page, "Irene Iris", "irene@comltd.xyz", "passwordi", "not_passwordi")
    UserHelper.is_signup_form_present(page).should be_true
  end

  it "displays signup form again if email is blank" do
    UserHelper.register(page, "Jack Jones", "", "passwordj")
    UserHelper.is_signup_form_present(page).should be_true
  end

  it "displays signup form again if password is empty" do
    UserHelper.register(page, "Liam Loh", "liam@comltd.xyz", "")
    UserHelper.is_signup_form_present(page).should be_true
  end

  it "displays signup form again if password is shorter than 6 chars long" do
    UserHelper.register(page, "Marjorie Mowlem", "marjorie@comltd.xyz", "just5")
    UserHelper.is_signup_form_present(page).should be_true
  end

  it "allows user to signup with a blank name" do
    UserHelper.register_login_and_logout(page, "", "kat@comltd.xyz", "passwordk")
  end

  it "displays signup form again if using existing email" do
    UserHelper.register_login_and_logout(page, "Nigel Noodle", "nigel@comp.xyz", "passwordn")
    UserHelper.register(page, "Nigel Noodle Again", "nigel@comp.xyz", "passwordn_again")
    UserHelper.is_signup_form_present(page).should be_true
  end

end
