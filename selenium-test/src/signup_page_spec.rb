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
require "src/email_helper"
include EmailHelper

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
    UserHelper.login_as_new_user(page, "bob@corp.xyz", "Bob Blue", "passwordb")
    begin
      UserHelper.is_logout_link_present_in_header(page).should be_true
    ensure
      UserHelper.logout(page)
    end
  end

  it "sends an email with a link after a successful registration" do
    UserHelper.register(page, "david@comp.xyz")
    EmailHelper.is_email_for("david@comp.xyz").should be_true
    EmailHelper.is_link_in_mail_for("david@comp.xyz").should be_true
  end

  it "displays signup form again if email is blank" do
    UserHelper.register(page, "")
    UserHelper.is_signup_form_present(page).should be_true
  end

  it "displays signup form again if using registered user's email" do
    UserHelper.register(page, "emma@comp.xyz")
    UserHelper.register(page, "emma@comp.xyz")
    UserHelper.is_signup_form_present(page).should be_true
  end
  
  it "displays signup form again if using activated user's email" do
    UserHelper.create_new_user(page, "fred@comp.xyz", "Fred Farrow", "passwordf")
    UserHelper.register(page, "fred@comp.xyz")
    UserHelper.is_signup_form_present(page).should be_true
  end

  it "has all footer links" do
    FooterHelper.are_footer_links_present(page).should be_true
  end
end
