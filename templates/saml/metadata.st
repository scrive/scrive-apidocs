###
samlMetadata=<?xml version="1.0" encoding="UTF-8"?>
<md:EntityDescriptor xmlns:md="urn:oasis:names:tc:SAML:2.0:metadata" validUntil="$validUntil$" entityID="$samlEntityBaseURI$">
   <md:SPSSODescriptor AuthnRequestsSigned="false" WantAssertionsSigned="true" protocolSupportEnumeration="urn:oasis:names:tc:SAML:2.0:protocol">
      <md:SingleLogoutService Binding="urn:oasis:names:tc:SAML:2.0:bindings:HTTP-Redirect" Location="$samlLogoutURI$" />
      <md:NameIDFormat>urn:oasis:names:tc:SAML:1.1:nameid-format:unspecified</md:NameIDFormat>
      <md:AssertionConsumerService Binding="urn:oasis:names:tc:SAML:2.0:bindings:HTTP-POST" Location="$samlACSURI$" index="1" />
      <md:AttributeConsumingService index="1">
         <md:ServiceName xml:lang="en">Scrive assertions consumer</md:ServiceName>
         <md:ServiceDescription xml:lang="en">Scrive SP assertions consumer requiring an email assured by IdP</md:ServiceDescription>
         <md:RequestedAttribute FriendlyName="email" Name="email" NameFormat="urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress" />
      </md:AttributeConsumingService>
   </md:SPSSODescriptor>
   <md:Organization>
      <md:OrganizationName xml:lang="en-US">Scrive AB</md:OrganizationName>
      <md:OrganizationDisplayName xml:lang="en-US">Scrive</md:OrganizationDisplayName>
      <md:OrganizationURL xml:lang="en-US">https://www.scrive.com</md:OrganizationURL>
   </md:Organization>
</md:EntityDescriptor>
###

