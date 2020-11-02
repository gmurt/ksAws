{*******************************************************************************
*                                                                              *
*  ksSNS - AWS SNS Interface                                                   *
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

unit ksAwsSns;

interface

uses
  Sysutils, Classes, ksAwsBase, System.Generics.Collections, Xml.XMLIntf;

type
  IksAwsSnsSubscription = interface
    ['{8640ABC9-89F5-407A-A60C-E3E9B4EA49E0}']
    function GetEndpoint: string;
    function GetOwner: string;
    function GetProtocol: string;
    function GetSubscriptionArn: string;
    function GetTopicArn: string;
    procedure SetEndpoint(const Value: string);
    procedure SetOwner(const Value: string);
    procedure SetProtocol(const Value: string);
    procedure SetSubscriptionArn(const Value: string);
    procedure SetTopicArn(const Value: string);
    procedure LoadFromXML(AXml: IXMLNode);
    property Endpoint: string read GetEndpoint write SetEndpoint;
    property Owner: string read GetOwner write SetOwner;
    property Protocol: string read GetProtocol write SetProtocol;
    property SubscriptionArn: string read GetSubscriptionArn write SetSubscriptionArn;
    property TopicArn: string read GetTopicArn write SetTopicArn;
  end;

  TksAwsSnsSubscriptionList = class(TList<IksAwsSnsSubscription>)
  public
    function AddSubscription: IksAwsSnsSubscription;
  end;

  IksAwsSNS = interface
  ['{8D5D089F-BA87-457D-BB51-693B551920C1}']
    procedure ListSubscriptions(ASubscriptions: TksAwsSnsSubscriptionList; const AMaxItems: Integer = 0);
    procedure ListTopics(ATopics: TStrings; const AMaxItems: integer = 0);
  end;

  function CreateSNS(AAccessKey, ASecretKey: string; ARegion: TksAwsRegion): IksAwsSNS;

implementation

uses ksAwsConst, Xml.xmldom, Xml.XMLDoc, Math, ksAwsHash;

type
  TksAwsSnsSubscription = class(TInterfacedObject, IksAwsSnsSubscription)
  private
    FEndpoint: string;
    FOwner: string;
    FProtocol: string;
    FSubscriptionArn: string;
    FTopicArn: string;
    function GetEndpoint: string;
    function GetOwner: string;
    function GetProtocol: string;
    function GetSubscriptionArn: string;
    function GetTopicArn: string;
    procedure SetEndpoint(const Value: string);
    procedure SetOwner(const Value: string);
    procedure SetProtocol(const Value: string);
    procedure SetSubscriptionArn(const Value: string);
    procedure SetTopicArn(const Value: string);
  protected
    procedure LoadFromXML(AXml: IXMLNode);
    property Endpoint: string read GetEndpoint write SetEndpoint;
    property Owner: string read GetOwner write SetOwner;
    property Protocol: string read GetProtocol write SetProtocol;
    property SubscriptionArn: string read GetSubscriptionArn write SetSubscriptionArn;
    property TopicArn: string read GetTopicArn write SetTopicArn;
  end;

  TksAwsSNS = class(TksAwsBaseService, IksAwsSNS)
  protected
    function GetApiVersion: string; override;
    function GetServiceName: string; override;
    procedure ListSubscriptions(ASubscriptions: TksAwsSnsSubscriptionList; const AMaxItems: Integer = 0);
    procedure ListTopics(ATopics: TStrings; const AMaxItems: integer = 0);
  public
    { Public declarations }
  end;


function CreateSNS(AAccessKey, ASecretKey: string; ARegion: TksAwsRegion): IksAwsSNS;
begin
  Result := TksAwsSNS.Create(AAccessKey, ASecretKey, ARegion);
end;

{ TksAwsSNS }

function TksAwsSNS.GetApiVersion: string;
begin
  Result := C_SNS_API_VERSION;
end;

function TksAwsSNS.GetServiceName: string;
begin
  Result := C_SERVICE_SNS;
end;


procedure TksAwsSNS.ListSubscriptions(ASubscriptions: TksAwsSnsSubscriptionList; const AMaxItems: Integer = 0);
var
  AResponse: string;
  AParams: TStrings;
  AXml: IXmlDocument;
  ANode: IXmlNode;
  ICount: integer;
  ANextToken: string;
  AComplete: Boolean;
  AMember: IXMLNode;
begin
  ASubscriptions.Clear;
  ANextToken := '';
  AComplete := False;
  AXml := TXMLDocument.Create(nil);
  while not AComplete do
  begin
    AParams := TStringList.Create;
    try
      if AMaxItems > 0 then
        AParams.Values['MaxItems'] := IntToStr(Min(AMaxItems, 100));
      AParams.Values['NextToken'] :=  ANextToken;
      AResponse := ExecuteHttp(C_GET, 'ListSubscriptions', Host, '', '', nil, AParams).ContentAsString;
    finally
      AParams.Free;
    end;
    AXml.LoadFromXML(AResponse);
    ANode := AXml.DocumentElement.ChildNodes['ListSubscriptionsResult'];
    ANextToken :=   UrlEncode(ANode.ChildNodes['NextToken'].Text);
    ANode := ANode.ChildNodes['Subscriptions'];
    for ICount := 0 to ANode.ChildNodes.Count -1 do
    begin
      AMember := ANode.ChildNodes[ICount];
      if (AMaxItems = 0) or (ASubscriptions.Count < AMaxItems) then
      begin
        ASubscriptions.AddSubscription.LoadFromXML(AMember);
      end;
    end;
    AComplete := (ANextToken = '') or (ASubscriptions.Count >= AMaxItems);
  end;
end;

procedure TksAwsSNS.ListTopics(ATopics: TStrings; const AMaxItems: integer = 0);
var
  AResponse: string;
  AParams: TStrings;
  AXml: IXmlDocument;
  ANode: IXmlNode;
  ICount: integer;
  ANextToken: string;
  AComplete: Boolean;
  ATopic: IXMLNode;
begin
  ATopics.Clear;
  ANextToken := '';
  AComplete := False;
  AXml := TXMLDocument.Create(nil);
  while not AComplete do
  begin
    AParams := TStringList.Create;
    try
      if AMaxItems > 0 then
        AParams.Values['MaxItems'] := IntToStr(Min(AMaxItems, 100));
      AParams.Values['NextToken'] :=  ANextToken;
      AResponse := ExecuteHttp(C_GET, 'ListTopics', Host, '', '', nil, AParams).ContentAsString;
    finally
      AParams.Free;
    end;
    AXml.LoadFromXML(AResponse);
    ANode := AXml.DocumentElement.ChildNodes['ListTopicsResult'];
    ANextToken :=   UrlEncode(ANode.ChildNodes['NextToken'].Text);
    ANode := ANode.ChildNodes['Topics'];
    for ICount := 0 to ANode.ChildNodes.Count -1 do
    begin
      ATopic := ANode.ChildNodes[ICount];
      if (AMaxItems = 0) or (ATopics.Count < AMaxItems) then
      begin
        ATopics.Add(ATopic.ChildNodes['TopicArn'].Text);
      end;
    end;
    AComplete := (ANextToken = '') or (ATopics.Count >= AMaxItems);
  end;
end;

{ TksAwsSnsSubscription }

function TksAwsSnsSubscription.GetEndpoint: string;
begin
  Result := FEndpoint;
end;

function TksAwsSnsSubscription.GetOwner: string;
begin
  Result := FOwner;
end;

function TksAwsSnsSubscription.GetProtocol: string;
begin
  Result := FProtocol;
end;

function TksAwsSnsSubscription.GetSubscriptionArn: string;
begin
  Result := FSubscriptionArn;
end;

function TksAwsSnsSubscription.GetTopicArn: string;
begin
  Result := FTopicArn;
end;

procedure TksAwsSnsSubscription.LoadFromXML(AXml: IXMLNode);
begin
  FEndpoint := AXml.ChildNodes['Endpoint'].Text;
  FOwner := AXml.ChildNodes['Owner'].Text;
  FProtocol := AXml.ChildNodes['Protocol'].Text;
  FSubscriptionArn := AXml.ChildNodes['SubscriptionArn'].Text;
  FTopicArn := AXml.ChildNodes['TopicArn'].Text;
end;

procedure TksAwsSnsSubscription.SetEndpoint(const Value: string);
begin
  FEndpoint := Value;
end;

procedure TksAwsSnsSubscription.SetOwner(const Value: string);
begin
  FOwner := Value;
end;

procedure TksAwsSnsSubscription.SetProtocol(const Value: string);
begin
  FProtocol := Value;
end;

procedure TksAwsSnsSubscription.SetSubscriptionArn(const Value: string);
begin
  FSubscriptionArn := Value;
end;

procedure TksAwsSnsSubscription.SetTopicArn(const Value: string);
begin
  FTopicArn := Value;
end;

{ TksAwsSnsSubscriptionList }

function TksAwsSnsSubscriptionList.AddSubscription: IksAwsSnsSubscription;
begin
  Result := TksAwsSnsSubscription.Create;
  Add(Result);
end;

end.


