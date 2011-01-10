require "rubygems"
gem "selenium-client"
require "selenium/client"

module FooterHelper

  def are_footer_links_present(page)
    page.is_element_present("//div[@id='footerContainer']//a[@href='/why.html']") and
    page.is_element_present("//div[@id='footerContainer']//a[@href='/features.html']") and
    page.is_element_present("//div[@id='footerContainer']//a[@href='/pricing.html']") and
    page.is_element_present("//div[@id='footerContainer']//a[@href='/security.html']") and
    page.is_element_present("//div[@id='footerContainer']//a[@href='/legal.html']") and
    page.is_element_present("//div[@id='footerContainer']//a[@href='/privacypolicy.html']") and
    page.is_element_present("//div[@id='footerContainer']//a[@href='/termsofuse.html']") and
    page.is_element_present("//div[@id='footerContainer']//a[@href='/contact.html']")
  end

end
