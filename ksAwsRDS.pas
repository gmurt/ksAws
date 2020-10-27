{*******************************************************************************
*                                                                              *
*  ksRDS - AWS RDS Interface                                                   *
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

unit ksAwsRDS;

interface

uses
  Sysutils, Classes, System.Generics.Collections, ksAwsBase;

type
  IksAwsRDSInstance = interface
    ['{6EF38F05-C2E3-455E-A87C-FD1C052F84F2}']
    function GetName: string;
    function GetStatus: string;
    property Name: string read GetName;
    property Status: string read GetStatus;
  end;

  TksAwsRDSInstanceList = class(TList<IksAwsRDSInstance>)
  public
    procedure AddInstanceXml(AXml: string);
  end;

  IksAwsRDS = interface
    ['{65C0D8F7-9C97-4693-8044-E494B5E6B5A0}']
    procedure ListInstances(AInstances: TksAwsRDSInstanceList);
  end;

  function CreateAwsRDS(APublicKey, APrivateKey: string; ARegion: TksAwsRegion): IksAwsRDS;


implementation

uses ksAwsHttpIntf, ksAwsConst, Xml.xmldom, Xml.XMLIntf, Xml.XMLDoc;

type
  TksAwsRDSInstance = class(TInterfacedObject, IksAwsRDSInstance)
  private
    FXml: IXmlDocument;
    FTags: TStrings;
    function GetName: string;
    function GetStatus: string;
  protected
    property Name: string read GetName;
    property Status: string read GetStatus;
  public
    constructor Create(AXml: string);
    destructor Destroy; override;
  end;

  TksAwsRDS = class(TksAwsBaseService, IksAwsRDS)
  private
    { Private declarations }
  protected
    function GetApiVersion: string; override;
    function GetServiceName: string; override;
    procedure ListInstances(AInstances: TksAwsRDSInstanceList);
  end;

function CreateAwsRDS(APublicKey, APrivateKey: string; ARegion: TksAwsRegion): IksAwsRDS;
begin
  Result := TksAwsRDS.Create(APublicKey, APrivateKey, ARegion);
end;

{ TksAwsRDS }

procedure TksAwsRDS.ListInstances(AInstances: TksAwsRDSInstanceList);
var
  AXml: IXMLDocument;
  AResponse: IXMLNode;
  AItems: IXMLNode;
  AItem: IXMLNode;
  ICount: integer;
begin
  AInstances.Clear;
  AXml := ExecuteHttpXml(C_GET, 'DescribeDBInstances', Host, '', '', nil, nil, True, nil);
  AResponse := AXml.ChildNodes['DescribeDBInstancesResponse'];
  AItems := AResponse.ChildNodes['DescribeDBInstancesResult'].ChildNodes['DBInstances'];
  for ICount := 0 to AItems.ChildNodes.Count-1 do
  begin
    AItem := AItems.ChildNodes[ICount];
    AInstances.AddInstanceXml(AItem.XML);
  end;
end;

function TksAwsRDS.GetApiVersion: string;
begin
  Result := C_RDS_API_VERSION;
end;

function TksAwsRDS.GetServiceName: string;
begin
  Result := C_SERVICE_RDS;
end;

{ TksAwsRDSInstance }

constructor TksAwsRDSInstance.Create(AXml: string);
begin
  FTags := TStringList.Create;
  FXml := TXmlDocument.Create(nil);
  FXml.LoadFromXML(AXml);
end;

destructor TksAwsRDSInstance.Destroy;
begin
  FTags.Free;
  inherited;
end;

function TksAwsRDSInstance.GetName: string;
begin
  Result := FXml.ChildNodes['DBInstance'].ChildNodes['DBInstanceIdentifier'].Text;
end;

function TksAwsRDSInstance.GetStatus: string;
begin
  Result := FXml.ChildNodes['DBInstance'].ChildNodes['DBInstanceStatus'].Text;
end;

{ TksAwsRDSInstanceList }

procedure TksAwsRDSInstanceList.AddInstanceXml(AXml: string);
var
  AInstance: IksAwsRDSInstance;
begin
  AInstance := TksAwsRDSInstance.Create(AXml);
  Add(AInstance);
end;

end.


