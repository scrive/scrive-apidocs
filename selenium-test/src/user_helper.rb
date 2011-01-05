require "rubygems"
gem "rspec"
gem "selenium-client"
require "selenium/client"
require "selenium/rspec/spec_helper"
require "spec/test/unit"

module UserHelper

  def register_login_and_logout(page, fullname, email, password) 
    register_login(page, fullname, email, password)
    logout(page)
  end

  def register_login(page, fullname, email, password) 
    register(page, fullname, email, password)
    login_and_accept_tos(page, email, password)
  end

  def login_and_logout(page, email, password)
    login(page, email, password)
    logout(page)
  end

  def register(page, fullname, email, password)
    register_with_second_password(page, fullname, email, password, password)
  end

  def register_with_second_password(page, fullname, email, password, password2)
    page.open "/signup"
    page.wait_for_page_to_load "30000"
    page.type "//form[@action='/signup']//input[@name='fullname']", fullname
    page.type "//form[@action='/signup']//input[@name='email']", email
    page.type "//form[@action='/signup']//input[@name='password']", password
    page.type "//form[@action='/signup']//input[@name='password2']", password2
    page.click "//form[@action='/signup']//input[@type='submit']"
    page.wait_for_page_to_load "30000"
  end

  def login_and_accept_tos(page, email, password)
    login(page, email, password)

    page.click "tos"
    page.click "//form[@action='/accepttos']//input[@type='submit']"
    page.wait_for_page_to_load "30000"
  end

  def login(page, email, password)
    page.open "/"
    page.wait_for_page_to_load "30000"
    page.type "//form[@action='/login']//input[@name='email']", email
    page.type "//form[@action='/login']//input[@name='password']", password
    page.click "login"
    page.wait_for_page_to_load "30000"
  end

  def logout(page)
    page.open "/"
    page.wait_for_page_to_load "30000"
    page.click "//a[@href='/logout']"
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

  def is_signup_link_present(page)
    page.is_element_present("//a[@href='/signup']")
  end 

  def is_password_remind_form_present(page)
    page.is_element_present("//form[@action='/amnesia']")
  end

  def is_logout_link_present_in_header(page)
    page.is_element_present("//div[@id='headerContainer']//a[@href='/logout']")
  end

  def is_signup_form_present(page)
    page.is_element_present("//form[@action='/signup']")
  end

end
