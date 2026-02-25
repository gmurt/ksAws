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
  Sysutils, Classes, ksAwsBase;

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
    function GetHtml: string;
    function GetRecipients: TStrings;
    function GetReplyTo: TStrings;
    function GetSender: string;
    function GetSubject: string;
    function GetReturnPath: string;
    procedure SetBody(const Value: string);
    procedure SetHtml(const Value: string);
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
    property Html: string read GetHtml write SetHtml;
  end;

  IksAwsSES = interface
  ['{95F8984E-2A05-4071-B906-67CDBCBEA51A}']
    function GetVerificationStatus(AEmail: string): string;
    function ListEmailIdentities: TEmailIdentities;

    procedure CreateVerificationTemplate(ATemplateName, AFromEmail, ASubject, ABody: string);
    procedure DeleteVerificationTemplate(ATemplateName: string);
    procedure SendVerificationTemplate(ATemplateName, ARecipient: string);

    procedure GetSenders(ASenders: TStrings; const AMaxItems: integer = 0);

    procedure DeleteIdentity(AIdentity: string);
    function SendEmail(AMessage: IksAwsSesMessage): integer;
    procedure VerifyEmailIdentity(AEmailAddress: string);
  end;

  function CreateSes(AAccessKey, ASecretKey: string; ARegion: TksAwsRegion): IksAwsSES;
  function CreateSesMessage(ARecipient, ASender, ASubject, ABody: string): IksAwsSesMessage; overload;
  function CreateSesMessage(ARecipients: TStrings; ASender, ASubject, ABody: string): IksAwsSesMessage; overload;


implementation

uses ksAwsConst, Math, ksAwsHash, JsonDataObjects, ksAwsXml;

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
  protected
    function GetApiVersion: string; override;
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
    procedure VerifyEmailIdentity(AEmailAddress: string);
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
  AHeaders: TStrings;
begin
  AJson := TJsonObject.Create;
  AHeaders := TStringList.Create;
  try
    AHeaders.Values['content-type'] := 'application/json';

    AJson.S['TemplateName'] := ATemplateName;
    AJson.S['FromEmailAddress'] := AFromEmail;
    AJson.S['TemplateSubject'] := ASubject;
    AJson.S['TemplateContent'] := ABody;
    AJson.S['SuccessRedirectionURL'] := '';
    AJson.S['FailureRedirectionURL'] := '';

    ExecuteHttp('POST', '', Host, '/v2/email/custom-verification-email-templates', AHeaders, nil, AJson.ToString);
  finally
    AJson.Free;
    AHeaders.Free;
  end;
end;

procedure TksAwsSES.DeleteVerificationTemplate(ATemplateName: string);
begin
    ExecuteHttp('DELETE', '', Host, '/v2/email/custom-verification-email-templates/'+ATemplateName, nil, nil, '');

end;

procedure TksAwsSES.SendVerificationTemplate(ATemplateName, ARecipient: string);
var
  AJson: TJsonObject;
  AHeaders: TStrings;
begin
  AJson := TJsonObject.Create;
  AHeaders := TStringList.Create;
  try
    AHeaders.Values['content-type'] := 'application/json';

    AJson.S['TemplateName'] := ATemplateName;
    AJson.S['EmailAddress'] := ARecipient;
    ExecuteHttp('POST', '', Host, '/v2/email/outbound-custom-verification-emails', AHeaders, nil, AJson.ToString);
  finally
    AJson.Free;
    AHeaders.Free;
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

function TksAwsSES.GetApiVersion: string;
begin
  Result := C_SES_API_VERSION;
end;

function TksAwsSES.GetHostSubdomain: string;
begin
  Result := 'email';
end;

procedure TksAwsSES.GetSenders(ASenders: TStrings; const AMaxItems: integer = 0);
var
  AResponse, AResult, AIdentitiesXml, AMemberXml: string;
  AParams: TStrings;
  ANextToken: string;
  AComplete: Boolean;
  AOffset, ABlockEnd: integer;
begin
  ASenders.Clear;
  ANextToken := '';
  AComplete := False;
  while not AComplete do
  begin
    AParams := TStringList.Create;
    try
      AParams.Values['IdentityType'] := 'EmailAddress';
      if AMaxItems > 0 then
        AParams.Values['MaxItems'] := IntToStr(Min(AMaxItems, 1000));
      if ANextToken <> '' then
        AParams.Values['NextToken'] := ANextToken;
      AResponse := ExecuteHttp('POST', 'ListIdentities', Host, '', nil, AParams, '').ContentAsString;
    finally
      AParams.Free;
    end;
    AResult := GetXmlTagValue(AResponse, 'ListIdentitiesResult');
    ANextToken := GetXmlTagValue(AResult, 'NextToken');
    AIdentitiesXml := GetXmlTagValue(AResult, 'Identities');
    AOffset := 1;
    while True do
    begin
      AMemberXml := GetXmlBlock(AIdentitiesXml, 'member', AOffset, ABlockEnd);
      if ABlockEnd = 0 then Break;
      if (AMaxItems = 0) or (ASenders.Count < AMaxItems) then
        ASenders.Add(AMemberXml);
      AOffset := ABlockEnd;
    end;
    AComplete := (ANextToken = '') or ((AMaxItems > 0) and (ASenders.Count >= AMaxItems));
    if not AComplete then
      Sleep(1000);
  end;
end;

function TksAwsSES.GetServiceName: string;
begin
  Result := C_SERVICE_SES;
end;

function TksAwsSES.GetVerificationStatus(AEmail: string): string;
var
  AResponse, AResultXml, AAttrsXml, AEntryXml, AValueXml: string;
  AParams: TStrings;
  ABlockEnd: integer;
begin
  Result := '';
  AParams := TStringList.Create;
  try
    AParams.Values['Identities.member.1'] := AEmail;
    AResponse := ExecuteHttp('POST', 'GetIdentityVerificationAttributes', Host, '', nil, AParams, '').ContentAsString;
  finally
    AParams.Free;
  end;
  AResultXml := GetXmlTagValue(AResponse, 'GetIdentityVerificationAttributesResult');
  AAttrsXml := GetXmlTagValue(AResultXml, 'VerificationAttributes');
  AEntryXml := GetXmlBlock(AAttrsXml, 'entry', 1, ABlockEnd);
  if ABlockEnd > 0 then
  begin
    AValueXml := GetXmlTagValue(AEntryXml, 'value');
    if AValueXml <> '' then
      Result := GetXmlTagValue(AValueXml, 'VerificationStatus');
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
      if ANextToken <> '' then
        AParams.Values['NextToken'] := ANextToken;
      AData := ExecuteHttp('GET', '', Host, '/v2/email/identities', nil, AParams, '').ContentAsString;

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
      end;

      ANextToken := '';
      if not AJson.IsNull('NextToken') then
        ANextToken := AJson.S['NextToken'];
    until ANextToken = '';
  finally
    AParams.Free;
    AJson.Free;
  end;
end;

function TksAwsSES.SendEmail(AMessage: IksAwsSesMessage): integer;
var
  AJson: TJsonObject;
  AHeaders: TStrings;
  AAddr: string;
begin
  AJson := TJsonObject.Create;
  AHeaders := TStringList.Create;
  try
    AHeaders.Values['content-type'] := 'application/json';

    AJson.O['Content'].O['Simple'].O['Subject'].S['Charset'] := 'UTF-8';
    AJson.O['Content'].O['Simple'].O['Subject'].S['Data'] := AMessage.Subject;

    if AMessage.Body <> '' then
    begin
      AJson.O['Content'].O['Simple'].O['Body'].O['Text'].S['Charset'] := 'UTF-8';
      AJson.O['Content'].O['Simple'].O['Body'].O['Text'].S['Data'] := AMessage.Body;
    end;

    if AMessage.Html <> '' then
    begin
      AJson.O['Content'].O['Simple'].O['Body'].O['Html'].S['Charset'] := 'UTF-8';
      AJson.O['Content'].O['Simple'].O['Body'].O['Html'].S['Data'] := AMessage.Html;
    end;

    for AAddr in AMessage.Recipients do
      AJson.O['Destination'].A['ToAddresses'].Add(AAddr);

    for AAddr in AMessage.CC do
      AJson.O['Destination'].A['CcAddresses'].Add(AAddr);

    for AAddr in AMessage.Bcc do
      AJson.O['Destination'].A['BccAddresses'].Add(AAddr);

    AJson.S['FromEmailAddress'] := AMessage.Sender;

    for AAddr in AMessage.ReplyTo do
      AJson.A['ReplyToAddresses'].Add(AAddr);

    if AMessage.ReturnPath <> '' then
      AJson.S['FeedbackForwardingEmailAddress'] := AMessage.ReturnPath;

    Result := ExecuteHttp('POST', '', Host, '/v2/email/outbound-emails', AHeaders, nil, AJson.ToString).StatusCode;
  finally
    AJson.Free;
    AHeaders.Free;
  end;
end;

procedure TksAwsSES.VerifyEmailIdentity(AEmailAddress: string);
var
  AParams: TStrings;
begin
  AParams := TStringList.Create;
  try
    AParams.Values['EmailAddress'] := AEmailAddress;
    ExecuteHttp('POST', 'VerifyEmailIdentity', Host, '', nil, AParams, '');
  finally
    AParams.Free;
  end;
end;


end.


