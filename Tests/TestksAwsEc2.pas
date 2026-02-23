unit TestksAwsEc2;

interface

uses
  DUnitX.TestFramework, Classes, SysUtils;

type
  [TestFixture]
  TTestEC2InstanceXmlParsing = class
  public
    [Test]
    procedure TestParseInstanceId;
    [Test]
    procedure TestParseInstanceStatus;
    [Test]
    procedure TestParseNameTag;
    [Test]
    procedure TestParseMissingNameTag;
    [Test]
    procedure TestParseMultipleTags;
  end;

  [TestFixture]
  TTestEC2InstanceList = class
  public
    [Test]
    procedure TestAddInstanceXml_IncreasesCount;
    [Test]
    procedure TestAddMultipleInstances;
    [Test]
    procedure TestClearList;
  end;

  [TestFixture]
  TTestEC2Factory = class
  public
    [Test]
    procedure TestCreateAwsEc2_ReturnsValidInterface;
    [Test]
    procedure TestInstanceList_Create;
  end;

implementation

uses
  ksAwsBase, ksAwsEc2;

const
  C_INSTANCE_XML =
    '<item>' +
    '<instanceId>i-1234567890abcdef0</instanceId>' +
    '<instanceState><code>16</code><name>running</name></instanceState>' +
    '<tagSet>' +
    '<item><key>Name</key><value>MyServer</value></item>' +
    '</tagSet>' +
    '</item>';

  C_INSTANCE_XML_NO_NAME_TAG =
    '<item>' +
    '<instanceId>i-abcdef1234567890</instanceId>' +
    '<instanceState><code>80</code><name>stopped</name></instanceState>' +
    '<tagSet>' +
    '<item><key>Environment</key><value>Production</value></item>' +
    '</tagSet>' +
    '</item>';

  C_INSTANCE_XML_MULTI_TAGS =
    '<item>' +
    '<instanceId>i-multi1234567890</instanceId>' +
    '<instanceState><code>16</code><name>running</name></instanceState>' +
    '<tagSet>' +
    '<item><key>Environment</key><value>Staging</value></item>' +
    '<item><key>Name</key><value>WebServer01</value></item>' +
    '<item><key>Team</key><value>Backend</value></item>' +
    '</tagSet>' +
    '</item>';

{ TTestEC2InstanceXmlParsing }

procedure TTestEC2InstanceXmlParsing.TestParseInstanceId;
var
  AList: TksAwsEC2InstanceList;
begin
  AList := TksAwsEC2InstanceList.Create;
  try
    AList.AddInstanceXml(C_INSTANCE_XML);
    Assert.AreEqual('i-1234567890abcdef0', AList[0].ID);
  finally
    AList.Free;
  end;
end;

procedure TTestEC2InstanceXmlParsing.TestParseInstanceStatus;
var
  AList: TksAwsEC2InstanceList;
begin
  AList := TksAwsEC2InstanceList.Create;
  try
    AList.AddInstanceXml(C_INSTANCE_XML);
    Assert.AreEqual('running', AList[0].Status);
  finally
    AList.Free;
  end;
end;

procedure TTestEC2InstanceXmlParsing.TestParseNameTag;
var
  AList: TksAwsEC2InstanceList;
begin
  AList := TksAwsEC2InstanceList.Create;
  try
    AList.AddInstanceXml(C_INSTANCE_XML);
    Assert.AreEqual('MyServer', AList[0].Name);
  finally
    AList.Free;
  end;
end;

procedure TTestEC2InstanceXmlParsing.TestParseMissingNameTag;
var
  AList: TksAwsEC2InstanceList;
begin
  AList := TksAwsEC2InstanceList.Create;
  try
    AList.AddInstanceXml(C_INSTANCE_XML_NO_NAME_TAG);
    Assert.AreEqual('', AList[0].Name);
  finally
    AList.Free;
  end;
end;

procedure TTestEC2InstanceXmlParsing.TestParseMultipleTags;
var
  AList: TksAwsEC2InstanceList;
begin
  AList := TksAwsEC2InstanceList.Create;
  try
    AList.AddInstanceXml(C_INSTANCE_XML_MULTI_TAGS);
    Assert.AreEqual('WebServer01', AList[0].Name);
  finally
    AList.Free;
  end;
end;

{ TTestEC2InstanceList }

procedure TTestEC2InstanceList.TestAddInstanceXml_IncreasesCount;
var
  AList: TksAwsEC2InstanceList;
begin
  AList := TksAwsEC2InstanceList.Create;
  try
    Assert.AreEqual(0, AList.Count);
    AList.AddInstanceXml(C_INSTANCE_XML);
    Assert.AreEqual(1, AList.Count);
  finally
    AList.Free;
  end;
end;

procedure TTestEC2InstanceList.TestAddMultipleInstances;
var
  AList: TksAwsEC2InstanceList;
begin
  AList := TksAwsEC2InstanceList.Create;
  try
    AList.AddInstanceXml(C_INSTANCE_XML);
    AList.AddInstanceXml(C_INSTANCE_XML_NO_NAME_TAG);
    AList.AddInstanceXml(C_INSTANCE_XML_MULTI_TAGS);
    Assert.AreEqual(3, AList.Count);
  finally
    AList.Free;
  end;
end;

procedure TTestEC2InstanceList.TestClearList;
var
  AList: TksAwsEC2InstanceList;
begin
  AList := TksAwsEC2InstanceList.Create;
  try
    AList.AddInstanceXml(C_INSTANCE_XML);
    AList.AddInstanceXml(C_INSTANCE_XML_NO_NAME_TAG);
    AList.Clear;
    Assert.AreEqual(0, AList.Count);
  finally
    AList.Free;
  end;
end;

{ TTestEC2Factory }

procedure TTestEC2Factory.TestCreateAwsEc2_ReturnsValidInterface;
var
  AService: IksAwsEC2;
begin
  AService := CreateAwsEc2('key', 'secret', awsUsEast1);
  Assert.IsNotNull(AService);
end;

procedure TTestEC2Factory.TestInstanceList_Create;
var
  AList: TksAwsEC2InstanceList;
begin
  AList := TksAwsEC2InstanceList.Create;
  try
    Assert.AreEqual(0, AList.Count);
  finally
    AList.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestEC2InstanceXmlParsing);
  TDUnitX.RegisterTestFixture(TTestEC2InstanceList);
  TDUnitX.RegisterTestFixture(TTestEC2Factory);

end.
