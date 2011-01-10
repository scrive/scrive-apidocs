require "rubygems"
gem "selenium-client"
require "selenium/client"
require "src/email_helper.rb"
include EmailHelper

module UserHelper

  def login_as_new_user(page, email, name, password)
    register(page, email)
    activate(page, email, name, password, password)  
  end

  def create_new_user(page, email, name, password)
    login_as_new_user(page, email, name, password)
    logout(page)
  end

  def register(page, email)
    page.open "/signup"
    page.wait_for_page_to_load "30000"
    page.type "//form[@action='']//input[@name='email']", email
    page.click "//form[@action='']//input[@type='submit']"
    page.wait_for_page_to_load "30000"
  end
  
  def activate(page, email, name, password, password2)
    follow_activation_link(page, email)
    tick_tos(page)
    
    page.type "//form[@action='']//input[@name='name']", name
    page.type "//form[@action='']//input[@name='password']", password
    page.type "//form[@action='']//input[@name='password2']", password2
    page.click "//form[@action='']//input[@type='submit']"
    page.wait_for_page_to_load "30000"
  end

  def follow_activation_link(page, email)
    link = EmailHelper.get_link_in_mail_for email
    page.open link
    page.wait_for_page_to_load "30000"
  end

  def tick_tos(page)
    page.click "tos"
    page.wait_for_element "//form//input[@type='submit']"
  end

  def change_password(page, email, password, password2)
    follow_change_password_link(page, email)
    
    page.type "//form[@action='']//input[@name='password']", password
    page.type "//form[@action='']//input[@name='password2']", password2
    page.click "//form[@action='']//input[@type='submit']"
    page.wait_for_page_to_load "30000"
  end

  def follow_change_password_link(page, email)
    link = EmailHelper.get_link_in_mail_for email
    page.open link
    page.wait_for_page_to_load "30000"
  end

  def login(page, email, password)
    login_with_header_form(page, email, password)
  end

  def login_with_header_form(page, email, password)
    page.type "//div[@id='headerContainer']//form[@action='/login']//input[@name='email']", email
    page.type "//div[@id='headerContainer']//form[@action='/login']//input[@name='password']", password
    page.click "//div[@id='headerContainer']//form[@action='/login']//input[@type='submit']"
    page.wait_for_page_to_load "30000"
  end

  def login_with_main_form(page, email, password)
    page.open "/login"
    page.wait_for_page_to_load "30000"  
    page.type "//div[@id='mainContainer']//form[@action='/login']//input[@name='email']", email
    page.type "//div[@id='mainContainer']//form[@action='/login']//input[@name='password']", password
    page.click "//div[@id='mainContainer']//form[@action='/login']//input[@type='submit']"
    page.wait_for_page_to_load "30000"
  end

  def logout(page)
    page.click "//div[@id='headerContainer']//a[@href='/logout']"
    page.wait_for_page_to_load "30000"  
  end

  def request_password_reminder(page, email)
    page.open "/amnesia"
    page.wait_for_page_to_load "30000"
    page.type "//form[@action='/amnesia']//input[@name='email']", email
    page.click "//form[@action='/amnesia']//input[@type='submit']"
    page.wait_for_page_to_load "30000"
  end

  def is_login_form_present_in_header(page)
    page.is_element_present("//div[@id='headerContainer']//form[@action='/login']")
  end

  def is_login_form_present_in_main(page)
    page.is_element_present("//div[@id='mainContainer']//form[@action='/login']")
  end

  def is_logout_link_present_in_header(page)
    page.is_element_present("//div[@id='headerContainer']//a[@href='/logout']")
  end

  def is_signup_link_present(page)
    page.is_element_present("//a[@href='/signup']")
  end 

  def is_signup_form_present(page)
    page.is_element_present "//form[@action='']"
    page.is_element_present "//form[@action='']//input[@name='email']"
  end

  def is_tos_present(page)
    page.is_element_present("tos")
  end

  def is_activation_form_present(page)
    is_tos_present(page) and page.is_element_present "//form//input[@type='submit']"
  end

  def is_password_remind_form_present(page)
    page.is_element_present("//form[@action='/amnesia']")
  end

  def is_change_password_form_present(page)
    page.is_element_present "//form[@action='']"
    page.is_element_present "//form[@action='']//input[@name='password']"
    page.is_element_present "//form[@action='']//input[@name='password2']"
  end
end
