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
  IksAwsSesMessage = interface
  ['{A58BC820-722F-4E68-8DE6-029A007FA29F}']
    function GetBcc: TStrings;
    function GetBody: string;
    function GetHtml: string;
    function GetCc: TStrings;
    function GetRecipients: TStrings;
    function GetSender: string;
    function GetSubject: string;
    procedure SetBody(const Value: string);
    procedure SetHtml(const Value: string);
    procedure SetSender(const Value: string);
    procedure SetSubject(const Value: string);
    property Sender: string read GetSender write SetSender;
    property Recipients: TStrings read GetRecipients;
    property CC: TStrings read GetCc;
    property Bcc: TStrings read GetBcc;
    property Subject: string read GetSubject write SetSubject;
    property Body: string read GetBody write SetBody;
    property Html: string read GetHtml write SetHtml;
  end;

  IksAwsSES = interface
  ['{95F8984E-2A05-4071-B906-67CDBCBEA51A}']
    procedure GetSenders(ASenders: TStrings; const ALimit: integer = 0);
    procedure DeleteIdentity(AIdentity: string);
    procedure SendEmail(AMessage: IksAwsSesMessage);
    procedure VerifyEmailIdentity(AEmailAddress: string);
  end;

  function CreateSes(AAccessKey, ASecretKey: string; ARegion: TksAwsRegion): IksAwsSES;
  function CreateSesMessage(ARecipient, ASender, ASubject, ABody: string; const AIsHtml: Boolean = True): IksAwsSesMessage;


implementation

uses ksAwsConst, Xml.xmldom, Xml.XMLIntf, Xml.XMLDoc, Math, ksAwsHash;

type
  TksAwsSesMessage = class(TInterfacedObject, IksAwsSesMessage)
  private
    FSender: string;
    FSubject: string;
    FBody: string;
    FHtml: string;
    FRecipients: TStrings;
    FCc: TStrings;
    FBcc: TStrings;
    function GetBcc: TStrings;
    function GetBody: string;
    function GetCc: TStrings;
    function GetRecipients: TStrings;
    function GetSender: string;
    function GetSubject: string;
    procedure SetBody(const Value: string);
    procedure SetSender(const Value: string);
    procedure SetSubject(const Value: string);
    function GetHtml: string;
    procedure SetHtml(const Value: string);
  protected
    property Sender: string read GetSender write SetSender;
    property Recipients: TStrings read GetRecipients;
    property CC: TStrings read GetCc;
    property Bcc: TStrings read GetBcc;
    property Subject: string read GetSubject write SetSubject;
    property Body: string read GetBody write SetBody;
    property Html: string read GetHtml write SetHtml;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TksAwsSES = class(TksAwsBaseService, IksAwsSES)
  private
    function BuildDestinationParams(AName: string; ARecipients, AParams: TStrings): string;
    { Private declarations }
  protected
    function GetServiceName: string; override;
    procedure DeleteIdentity(AIdentity: string);
    procedure GetSenders(ASenders: TStrings; const AMaxItems: integer = 0);
    procedure SendEmail(AMessage: IksAwsSesMessage);
    procedure VerifyEmailIdentity(AEmailAddress: string);
  public
    { Public declarations }
  end;


function CreateSes(AAccessKey, ASecretKey: string; ARegion: TksAwsRegion): IksAwsSES;
begin
  Result := TksAwsSES.Create(AAccessKey, ASecretKey, ARegion);
end;

function CreateSesMessage(ARecipient, ASender, ASubject, ABody: string; const AIsHtml: Boolean = True): IksAwsSesMessage;
begin
  Result := TksAwsSesMessage.Create;
  Result.Recipients.Add(ARecipient);
  Result.Sender := ASender;
  Result.Subject := ASubject;
  case AIsHtml of
    True: Result.Html := ABody;
    False: Result.Body := ABody;
  end;
end;

{ TksAwsSesMessage }

constructor TksAwsSesMessage.Create;
begin
  FRecipients := TStringList.Create;
  FCc := TStringList.Create;
  FBcc := TStringList.Create;
end;

destructor TksAwsSesMessage.Destroy;
begin
  FRecipients.Free;
  FCc.Free;
  FBcc.Free;
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

procedure TksAwsSesMessage.SetSender(const Value: string);
begin
  FSender := Value;
end;

procedure TksAwsSesMessage.SetSubject(const Value: string);
begin
  FSubject := Value;
end;

{ TksAwsSES }

procedure TksAwsSES.DeleteIdentity(AIdentity: string);
var
  AParams: TStrings;
begin
  AParams := TStringList.Create;
  try
    AParams.Values['Identity'] := AIdentity;
    ExecuteHttp('DELETE', 'DeleteIdentity', Host, '', '', nil, AParams);
  finally
    AParams.Free;
  end;
end;

function TksAwsSES.BuildDestinationParams(AName: string; ARecipients, AParams: TStrings): string;
var
  ICount: integer;
  AKey: string;
begin
  if LowerCase(AName) = 'to' then AKey := 'ToAddresses';
  if LowerCase(AName) = 'cc' then AKey := 'CcAddresses';
  if LowerCase(AName) = 'bcc' then AKey := 'BccAddresses';
  for ICount := 1 to ARecipients.Count do
    AParams.Values['Destination.'+AKey+'.member.'+IntToStr(ICount)] := ARecipients[ICount-1];
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
      AResponse := ExecuteHttp('POST', 'ListIdentities', Host, '', '', nil, AParams).ContentAsString;
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

procedure TksAwsSES.SendEmail(AMessage: IksAwsSesMessage);
var
  AResponse: string;
  AParams: TStrings;
begin
  AParams := TStringList.Create;
  try
    AParams.Values['Source'] := AMessage.Sender;
    BuildDestinationParams('to', AMessage.Recipients, AParams);
    BuildDestinationParams('cc', AMessage.CC, AParams);
    BuildDestinationParams('bcc', AMessage.Bcc, AParams);
    AParams.Values['Message.Subject.Data'] := AMessage.Subject;
    AParams.Values['Message.Body.Text.Data'] := AMessage.Body;
    AParams.Values['Message.Body.Html.Data'] := AMessage.Html;
    AResponse := ExecuteHttp('POST', 'SendEmail', Host, '', '', nil, AParams).ContentAsString;
  finally
    AParams.Free;
  end;
end;

procedure TksAwsSES.VerifyEmailIdentity(AEmailAddress: string);
var
  AResponse: string;
  AParams: TStrings;
begin
  AParams := TStringList.Create;
  try
    AParams.Values['EmailAddress'] := AEmailAddress;
    AResponse := ExecuteHttp('POST', 'VerifyEmailIdentity', Host, '', '', nil, AParams).ContentAsString;
  finally
    AParams.Free;
  end;
end;


end.


