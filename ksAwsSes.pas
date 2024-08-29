{*******************************************************************************
*                                                                              *
*  ksSES - AWS SES Interface                                                   *
*                                                                              *
*  https://github.com/gmurt/ksAWS                                            *
*                                                                              *
*  Copyright 2020 Graham Murt                                                  *
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
  Sysutils, Classes, ksAwsBase, IdMessage;

type
  TEmailIdentity = record
    Identity: string;
    &Type: string;
    Verified: Boolean;
  end;

  TEmailIdentities = TArray<TEmailIdentity>;

  IksAwsSesMessage = interface
  ['{A58BC820-722F-4E68-8DE6-029A007FA29F}']
    function GetBcc: TStrings;
    function GetBody: string;
    function GetCc: TStrings;
    function GetRecipients: TStrings;
    function GetReplyTo: TStrings;
    function GetSender: string;
    function GetSubject: string;
    function GetReturnPath: string;
    procedure SetBody(const Value: string);
    procedure SetSender(const Value: string);
    procedure SetSubject(const Value: string);
    procedure SetReturnPath(const Value: string);
    property Sender: string read GetSender write SetSender;
    property Recipients: TStrings read GetRecipients;
    property ReplyTo: TStrings read GetReplyTo;
    property ReturnPath: string read GetReturnPath write SetReturnPath;
    property CC: TStrings read GetCc;
    property Bcc: TStrings read GetBcc;
    property Subject: string read GetSubject write SetSubject;
    property Body: string read GetBody write SetBody;

  end;

  IksAwsSES = interface
  ['{95F8984E-2A05-4071-B906-67CDBCBEA51A}']
    function GetVerificationStatus(AEmail: string): string;
    function ListEmailIdentities: TEmailIdentities;

    procedure CreateVerificationTemplate(ATemplateName, AFromEmail, ASubject, ABody: string);
    procedure DeleteVerificationTemplate(ATemplateName: string);
    procedure SendVerificationTemplate(ATemplateName, ARecipient: string);

    procedure GetSenders(ASenders: TStrings; const ALimit: integer = 0);

    procedure DeleteIdentity(AIdentity: string);
    function SendEmail(AMessage: IksAwsSesMessage): integer;
    procedure SendEmailSmtp(AMsg: TIdMessage);
    procedure VerifyEmailIdentity(AEmailAddress: string);
  end;

  function CreateSes(AAccessKey, ASecretKey: string; ARegion: TksAwsRegion): IksAwsSES;
  function CreateSesMessage(ARecipient, ASender, ASubject, ABody: string): IksAwsSesMessage; overload;
  function CreateSesMessage(ARecipients: TStrings; ASender, ASubject, ABody: string): IksAwsSesMessage; overload;


implementation

uses ksAwsConst, Xml.xmldom, Xml.XMLIntf, Xml.XMLDoc, Math, ksAwsHash, JsonDataObjects,
  IdSmtp,  IdSSL, IdSSLOpenSSL, IdExplicitTLSClientServerBase;

type
  TksAwsSesMessage = class(TInterfacedObject, IksAwsSesMessage)
  private
    FSender: string;
    FSubject: string;
    FBody: string;
    FHtml: string;
    FRecipients: TStrings;
    FReplyTo: TStrings;
    FCc: TStrings;
    FBcc: TStrings;
    FReturnPath: string;
    function GetBcc: TStrings;
    function GetBody: string;
    function GetCc: TStrings;
    function GetRecipients: TStrings;
    function GetReplyTo: TStrings;
    function GetSender: string;
    function GetSubject: string;
    procedure SetBody(const Value: string);
    procedure SetSender(const Value: string);
    procedure SetSubject(const Value: string);
    function GetHtml: string;
    procedure SetHtml(const Value: string);
    function GetReturnPath: string;
    procedure SetReturnPath(const Value: string);
  protected
    property Sender: string read GetSender write SetSender;
    property Recipients: TStrings read GetRecipients;
    property CC: TStrings read GetCc;
    property Bcc: TStrings read GetBcc;
    property ReturnPath: string read GetReturnPath write SetReturnPath;
    property Subject: string read GetSubject write SetSubject;
    property Body: string read GetBody write SetBody;
    property Html: string read GetHtml write SetHtml;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TksAwsSES = class(TksAwsBaseService, IksAwsSES)
  private
    //function BuildDestinationParams(AName: string; ARecipients, AParams: TStrings): string;
    { Private declarations }
  protected
    function GetServiceName: string; override;
    function GetHostSubdomain: string; override;
    function GetVerificationStatus(AEmail: string): string; overload;
    function ListEmailIdentities: TEmailIdentities;

    procedure CreateVerificationTemplate(ATemplateName, AFromEmail, ASubject, ABody: string);
    procedure DeleteVerificationTemplate(ATemplateName: string);
    procedure SendVerificationTemplate(ATemplateName, ARecipient: string);
    procedure DeleteIdentity(AIdentity: string);
    procedure GetSenders(ASenders: TStrings; const AMaxItems: integer = 0);
    function SendEmail(AMessage: IksAwsSesMessage): integer;
    procedure SendEmailSmtp(AMessage: TIdMessage);
    procedure VerifyEmailIdentity(AEmailAddress: string);

  public
    { Public declarations }
  end;


function CreateSes(AAccessKey, ASecretKey: string; ARegion: TksAwsRegion): IksAwsSES;
begin
  Result := TksAwsSES.Create(AAccessKey, ASecretKey, ARegion);
end;

function CreateSesMessage(ARecipient, ASender, ASubject, ABody: string): IksAwsSesMessage;
var
  ARecipients: TStrings;
begin
  ARecipients := TStringList.Create;
  try
    ARecipients.Add(ARecipient);
    Result := CreateSesMessage(ARecipients, ASender, ASubject, ABody);
  finally
    ARecipients.Free;
  end;
end;

function CreateSesMessage(ARecipients: TStrings; ASender, ASubject, ABody: string): IksAwsSesMessage;
var
  ARecipient: string;
begin
  Result := TksAwsSesMessage.Create;
  for ARecipient in ARecipients do
    Result.Recipients.Add(ARecipient);
  Result.Sender := ASender;
  Result.Subject := ASubject;
  Result.Body := ABody;
end;

{ TksAwsSesMessage }

constructor TksAwsSesMessage.Create;
begin
  FRecipients := TStringList.Create;
  FCc := TStringList.Create;
  FBcc := TStringList.Create;
  FReplyTo := TStringList.Create;
  FReturnPath := '';
end;

destructor TksAwsSesMessage.Destroy;
begin
  FRecipients.Free;
  FCc.Free;
  FBcc.Free;
  FReplyTo.Free;
  inherited;
end;

function TksAwsSesMessage.GetBcc: TStrings;
begin
  Result := FBcc;
end;

function TksAwsSesMessage.GetBody: string;
begin
  Result := FBody;
end;

function TksAwsSesMessage.GetCc: TStrings;
begin
  Result := FCc;
end;

function TksAwsSesMessage.GetHtml: string;
begin
  Result := FHtml;
end;

function TksAwsSesMessage.GetRecipients: TStrings;
begin
  Result := FRecipients;
end;

function TksAwsSesMessage.GetReplyTo: TStrings;
begin
  Result := FReplyTo;
end;

function TksAwsSesMessage.GetReturnPath: string;
begin
  Result := FReturnPath;
end;

function TksAwsSesMessage.GetSender: string;
begin
  Result := FSender;
end;

function TksAwsSesMessage.GetSubject: string;
begin
  Result := FSubject;
end;


procedure TksAwsSesMessage.SetBody(const Value: string);
begin
  FBody := Value;
end;

procedure TksAwsSesMessage.SetHtml(const Value: string);
begin
  FHtml := Value;
end;

procedure TksAwsSesMessage.SetReturnPath(const Value: string);
begin
  FReturnPath := Value;
end;

procedure TksAwsSesMessage.SetSender(const Value: string);
begin
  FSender := Value;
end;

procedure TksAwsSesMessage.SetSubject(const Value: string);
begin
  FSubject := Value;
end;

{ TksAwsSES }

procedure TksAwsSES.CreateVerificationTemplate(ATemplateName, AFromEmail, ASubject, ABody: string);
var
  AJson: TJsonObject;

begin
  AJson := TJsonObject.Create;
  try
    AJson.S['TemplateName'] := ATemplateName;
    AJson.S['FromEmailAddress'] := AFromEmail;
    AJson.S['TemplateSubject'] := ASubject;
    AJson.S['TemplateContent'] := ABody;
    AJson.S['SuccessRedirectionURL'] := '';
    AJson.S['FailureRedirectionURL'] := '';


    ExecuteHttp('POST', 'CreateCustomVerificationEmailTemplate', Host, '/v2/email/custom-verification-email-templates',  nil, nil, AJson.ToString);
  finally
    AJson.Free;
  end;
end;

procedure TksAwsSES.DeleteVerificationTemplate(ATemplateName: string);
begin
    ExecuteHttp('DELETE', 'DeleteCustomVerificationEmailTemplate', Host, '/v2/email/custom-verification-email-templates/'+ATemplateName, nil, nil, '');

end;

procedure TksAwsSES.SendVerificationTemplate(ATemplateName, ARecipient: string);
var
  AJson: TJsonObject;

begin
  AJson := TJsonObject.Create;
  try
    AJson.S['TemplateName'] := ATemplateName;
    AJson.S['EmailAddress'] := ARecipient;
    ExecuteHttp('POST', 'SendCustomVerificationEmail', Host, '/v2/email/outbound-custom-verification-emails',  nil, nil, AJson.ToString);
  finally
    AJson.Free;
  end;
end;

procedure TksAwsSES.DeleteIdentity(AIdentity: string);
var
  AParams: TStrings;
begin
  AParams := TStringList.Create;
  try
    AParams.Values['Identity'] := AIdentity;
    ExecuteHttp('DELETE', 'DeleteIdentity', Host, '', nil, AParams, '');
  finally
    AParams.Free;
  end;
end;

{function TksAwsSES.BuildDestinationParams(AName: string; ARecipients, AParams: TStrings): string;
var
  ICount: integer;
  AKey: string;
begin
  if LowerCase(AName) = 'to' then AKey := 'ToAddresses';
  if LowerCase(AName) = 'cc' then AKey := 'CcAddresses';
  if LowerCase(AName) = 'bcc' then AKey := 'BccAddresses';
  for ICount := 1 to ARecipients.Count do
    AParams.Values['Destination.'+AKey+'.member.'+IntToStr(ICount)] := ARecipients[ICount-1];
end;}

function TksAwsSES.GetHostSubdomain: string;
begin
  Result := 'email';
end;

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
      AParams.Values['IdentityType'] := 'EmailAddress';
      if AMaxItems > 0 then
        AParams.Values['MaxItems'] := IntToStr(Min(AMaxItems, 1000));
      AParams.Values['NextToken'] :=  ANextToken;
      AResponse := ExecuteHttp('POST', 'ListIdentities', Host, '', nil, AParams, '').ContentAsString;
    finally
      AParams.Free;
    end;
    AXml.LoadFromXML(AResponse);
    AIdentities := AXml.DocumentElement.ChildNodes['ListIdentitiesResult'];
    ANextToken :=   UrlEncode(AIdentities.ChildNodes['NextToken'].Text);
    AIdentities := AIdentities.ChildNodes['Identities'];
    for ICount := 0 to AIdentities.ChildNodes.Count -1 do
    begin
      if (AMaxItems = 0) or (ASenders.Count < AMaxItems) then
      begin
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

function TksAwsSES.GetVerificationStatus(AEmail: string): string;
var
  AResponse: string;
  AParams: TStrings;
  ICount: integer;
  AStrings: TStrings;
  AStr: string;
begin
  Result := '';
  AStrings := TStringList.Create;
  AParams := TStringList.Create;
  try
    AParams.Values['Identities.member.'+IntToStr(1)] := AEmail;
    AResponse := ExecuteHttp('POST', 'GetIdentityVerificationAttributes', Host, '', nil, AParams, '').ContentAsString;
    AStrings.Text := AResponse;
    for ICount := 0 to AStrings.Count-1 do
    begin
      AStr := Trim(AStrings[ICount]).ToLower;
      if Pos('<verificationstatus>', AStr) > 0 then
      begin
        AStr := StringReplace(AStr, '<verificationstatus>', '', []);
        AStr := StringReplace(AStr, '</verificationstatus>', '', []);
        Result := AStr;
        Exit;
      end;
    end;
  finally
    AParams.Free;
    AStrings.Free;
  end;
end;

function TksAwsSES.ListEmailIdentities: TEmailIdentities;
var
  AData: string;
  AJson: TJsonObject;
  AParams: TStrings;
  ANextToken: string;
  AObj: TJsonObject;
  AIdentity: TEmailIdentity;
begin
  Result := [];

  AParams := TStringList.Create;
  AJson := TJsonObject.Create;
  try
    AParams.Values['PageSize'] := '1000';
    ANextToken := '';
    repeat
      AData := ExecuteHttp('GET', 'ListEmailIdentities', Host, '/v2/email/identities', nil, AParams, '').ContentAsString;

      AJson.FromJSON(AData);

      for AObj in AJson.A['EmailIdentities'] do
      begin

        if AObj.B['SendingEnabled'] then
        begin
          AIdentity.Identity := AObj.S['IdentityName'];
          AIdentity.&Type := AObj.S['IdentityType'];
          AIdentity.Verified := AObj.B['SendingEnabled'];
          Result := Result + [AIdentity];
        end;
        //  AResult.Add(AIdentity.S['IdentityName']);
      end;

      if not AJson.IsNull('NextToken') then
        ANextToken := AJson.S['NextToken'];
    until ANextToken = '';
  finally
    AParams.Free;
    AJson.Free;
  end;
end;

function  TksAwsSES.SendEmail(AMessage: IksAwsSesMessage): integer;
var
  AJson: TJsonObject;
  AAddr: string;
begin
  AJson := TJsonObject.Create;
  try
    AJson.O['Content'].O['Simple'].O['Body'].O['Html'].S['Charset'] := 'UTF-8';
    AJson.O['Content'].O['Simple'].O['Body'].O['Html'].S['Data'] := AMessage.Body;
    AJson.O['Content'].O['Simple'].O['Subject'].S['Charset'] := 'UTF-8';
    AJson.O['Content'].O['Simple'].O['Subject'].S['Data'] := AMessage.Subject;

    for AAddr in AMessage.Recipients do
      AJson.O['Destination'].A['ToAddresses'].Add(AAddr);

    for AAddr in AMessage.CC do
      AJson.O['Destination'].A['CcAddresses'].Add(AAddr);

    for AAddr in AMessage.Bcc do
      AJson.O['Destination'].A['BccAddresses'].Add(AAddr);

    AJson.S['FromEmailAddress'] := AMessage.Sender;

    for AAddr in AMessage.ReplyTo do
      AJson.A['ReplyToAddresses'].Add(AAddr);

    Result := ExecuteHttp('POST', 'SendEmail', Host, '/v2/email/outbound-emails',  nil, nil, AJson.ToString).StatusCode;
  finally
    AJson.Free;
  end;
end;

procedure TksAwsSES.SendEmailSmtp(AMessage: TIdMessage);
var
  ASmtp: TIdSMTP;
  ASSl: TIdSSLIOHandlerSocketOpenSSL;
begin
  {ASmtp := TIdSMTP.Create(nil);
  ASsl := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  try
    //AMessage.Sender.
    ASmtp.Host := 'email-smtp.'+RegionStr+'.amazonaws.com';
    ASmtp.Port := C_AMAZON_PORT;
    ASmtp.Username := C_AMAZON_USER;
    ASmtp.Password := C_AMAZON_PASS;
    ASmtp.IOHandler := ASsl;
    ASmtp.UseTLS := utUseExplicitTLS;
    ASsl.SSLOptions.Method := TIdSSLVersion.sslvTLSv1_2;
    ASsl.SSLOptions.SSLVersions := [sslvTLSv1,sslvTLSv1_1,sslvTLSv1_2];
    ASmtp.Connect;
    try
      ASmtp.Send(AMessage);
    finally
      ASmtp.Disconnect;
    end;

  finally
    ASmtp.Free;
    ASsl.Free;
  end; }
end;

procedure TksAwsSES.VerifyEmailIdentity(AEmailAddress: string);
var
  AResponse: string;
  AParams: TStrings;
begin
  AParams := TStringList.Create;
  try
    AParams.Values['EmailAddress'] := AEmailAddress;
    AResponse := ExecuteHttp('POST', 'VerifyEmailIdentity', Host, '', nil, AParams, '').ContentAsString;
  finally
    AParams.Free;
  end;
end;


end.


