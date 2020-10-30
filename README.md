# ksAws
Delphi interfaces for Amazon Web Services. Tested with XE4 and Delphi 10.3. The interfaces will use THttpClient if available, if not it will revert back to Indy for earlier versions of Delphi.

These are not complete implementations of the Amazon APIs, I am just adding functionality as I require it within my own apps.  If you'd like me add any methods which are not currently supported, let me know. 


Included Interfaces
-------------------
ksSes - Simple email service interface

ksS3  - Amazon S3 storage interface

ksEC2 - EC2 virtual server interface

ksRDS - RDS database service interface
