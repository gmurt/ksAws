{*******************************************************************************
*                                                                              *
*  ksSES - AWS SES Interface                                                   *
*                                                                              *
*  https://github.com/gmurt/ksSES                                              *
*                                                                              *
*  Copyright 2015 Graham Murt                                                  *
*                                                                              *
*  email: graham@kernow-software.co.uk                                         *
*                                                                              *
*  Licensed under the Apache License, Version 2.0 (the "License");             *
*  you may not use this file except in compliance with the License.            *
*  You may obtain a copy of the License at                                     *
*                                                                              *
*    http://www.apache.org/licenses/LICENSE-2.0                                *
*                                                                              *
*  Unless required by applicable law or agreed to in writing, software         *
*  distributed under the License is distributed on an "AS IS" BASIS,           *
*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    *
*  See the License for the specific language governing permissions and         *
*  limitations under the License.                                              *
*                                                                              *
*******************************************************************************}


unit ksAwsSes;

interface

uses
  Sysutils, Classes, ksAwsBase;

type


  IksAwsSES = interface
  ['{95F8984E-2A05-4071-B906-67CDBCBEA51A}']
    procedure GetSenders(ASenders: TStrings; const ALimit: integer = 0);
    procedure VerifyEmailIdentity(AEmailAddress: string);
  end;

  function CreateSes(APublicKey, APrivateKey: string; ARegion: TksAwsRegion): IksAwsSES;


implementation

uses ksAwsConst, Net.HttpClient, Xml.xmldom, Xml.XMLIntf, Xml.XMLDoc, System.NetEncoding,
  Math;

type
  TksAwsSES = class(TksAwsBaseService, IksAwsSES)
  private
    { Private declarations }
  protected
    function GetServiceName: string; override;
    procedure GetSenders(ASenders: TStrings; const AMaxItems: integer = 0);
    procedure VerifyEmailIdentity(AEmailAddress: string);
  public
    { Public declarations }
  end;


function CreateSes(APublicKey, APrivateKey: string; ARegion: TksAwsRegion): IksAwsSES;
begin
  Result := TksAwsSES.Create(APublicKey, APrivateKey, ARegion);
end;


{ TksAwsSES }

procedure TksAwsSES.GetSenders(ASenders: TStrings; const AMaxItems: integer = 0);
var
  AResponse: string;
  AParams: TStrings;
  AXml: IXmlDocument;
  AIdentities: IXmlNode;
  ICount: integer;
  ANextToken: string;
  AComplete: Boolean;
begin
  ASenders.Clear;
  ANextToken := '';
  AComplete := False;
  AXml := TXMLDocument.Create(nil);
  while not AComplete do
  begin
    AParams := TStringList.Create;
    try
      AParams.Values['Action'] := 'ListIdentities';
      AParams.Values['IdentityType'] := 'EmailAddress';
      if AMaxItems > 0 then
        AParams.Values['MaxItems'] := IntToStr(Min(AMaxItems, 1000));
      AParams.Values['NextToken'] :=  ANextToken;
      AResponse := ExecuteHttp('POST', Host, '', '', nil, AParams).ContentText;
    finally
      AParams.Free;
    end;
    AXml.LoadFromXML(AResponse);
    AIdentities := AXml.DocumentElement.ChildNodes['ListIdentitiesResult'];
    ANextToken :=  TNetEncoding.URL.Encode(AIdentities.ChildNodes['NextToken'].Text);
    AIdentities := AIdentities.ChildNodes['Identities'];
    for ICount := 0 to AIdentities.ChildNodes.Count -1 do
    begin
      if (AMaxItems = 0) or (ASenders.Count < AMaxItems) then
      begin
        //if ASenders.IndexOf(AIdentities.ChildNodes[ICount].Text) = -1 then
          ASenders.Add(AIdentities.ChildNodes[ICount].Text);
      end;
    end;
    AComplete := (ANextToken = '') or (ASenders.Count = AMaxItems);
    if not AComplete then
      Sleep(1000); // rate limit for AWS (no more than 1 call per second);
  end;
end;

function TksAwsSES.GetServiceName: string;
begin
  Result := C_SERVICE_SES;
end;

procedure TksAwsSES.VerifyEmailIdentity(AEmailAddress: string);
var
  AResponse: string;
  AParams: TStrings;
begin
  AParams := TStringList.Create;
  try
    AParams.Values['Action'] := 'VerifyEmailIdentity';
    AParams.Values['EmailAddress'] := StringReplace(AEmailAddress, '@', '%40', []);// TNetEncoding.URL.Encode(AEmailAddress);
    AResponse := ExecuteHttp('POST', Host, '', '', nil, AParams).ContentText;
  finally
    AParams.Free;
  end;
end;

end.


