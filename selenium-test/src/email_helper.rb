require "rubygems"
gem "rspec"
require "selenium-webdriver"

class EmailHelper

  def initialize(ctx, driver, wait)
    @driver = driver
    @ctx = ctx
    @wait = wait

    @loginhelper = LoginHelper.new(@ctx, @driver, @wait)
  end

  def follow_link_in_latest_mail_for email

    link = ""
    @loginhelper.login_as(@ctx.props.tester_email, @ctx.props.tester_password)
    begin
      @driver.navigate().to(@ctx.createKontrakcjaURL("/dave/backdoor/" + email))
      @wait.until { @driver.find_element :css => ".mainContainer a" }
      link = (@driver.find_elements :css => ".mainContainer a").first.attribute("href")
      puts "Link from latest email to " + email + " has link " + link
    ensure
      @loginhelper.logout
    end
    @driver.navigate().to(link)
  end
end
