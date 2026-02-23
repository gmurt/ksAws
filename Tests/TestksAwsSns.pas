unit TestksAwsSns;

interface

uses
  DUnitX.TestFramework, Classes, SysUtils;

type
  [TestFixture]
  TTestSnsSubscriptionProperties = class
  public
    [Test]
    procedure TestEndpoint_SetGet;
    [Test]
    procedure TestOwner_SetGet;
    [Test]
    procedure TestProtocol_SetGet;
    [Test]
    procedure TestSubscriptionArn_SetGet;
    [Test]
    procedure TestTopicArn_SetGet;
  end;

  [TestFixture]
  TTestSnsSubscriptionLoadFromXml = class
  public
    [Test]
    procedure TestLoadFromXml_AllFields;
    [Test]
    procedure TestLoadFromXml_EmailProtocol;
  end;

  [TestFixture]
  TTestSnsSubscriptionList = class
  public
    [Test]
    procedure TestAddSubscription_ReturnsInterface;
    [Test]
    procedure TestAddSubscription_IncreasesCount;
    [Test]
    procedure TestAddMultipleSubscriptions;
    [Test]
    procedure TestClearList;
  end;

  [TestFixture]
  TTestSnsFactory = class
  public
    [Test]
    procedure TestCreateSNS_ReturnsValidInterface;
  end;

implementation

uses
  ksAwsBase, ksAwsSns;

{ TTestSnsSubscriptionProperties }

procedure TTestSnsSubscriptionProperties.TestEndpoint_SetGet;
var
  AList: TksAwsSnsSubscriptionList;
  ASub: IksAwsSnsSubscription;
begin
  AList := TksAwsSnsSubscriptionList.Create;
  try
    ASub := AList.AddSubscription;
    ASub.Endpoint := 'user@example.com';
    Assert.AreEqual('user@example.com', ASub.Endpoint);
  finally
    AList.Free;
  end;
end;

procedure TTestSnsSubscriptionProperties.TestOwner_SetGet;
var
  AList: TksAwsSnsSubscriptionList;
  ASub: IksAwsSnsSubscription;
begin
  AList := TksAwsSnsSubscriptionList.Create;
  try
    ASub := AList.AddSubscription;
    ASub.Owner := '123456789012';
    Assert.AreEqual('123456789012', ASub.Owner);
  finally
    AList.Free;
  end;
end;

procedure TTestSnsSubscriptionProperties.TestProtocol_SetGet;
var
  AList: TksAwsSnsSubscriptionList;
  ASub: IksAwsSnsSubscription;
begin
  AList := TksAwsSnsSubscriptionList.Create;
  try
    ASub := AList.AddSubscription;
    ASub.Protocol := 'email';
    Assert.AreEqual('email', ASub.Protocol);
  finally
    AList.Free;
  end;
end;

procedure TTestSnsSubscriptionProperties.TestSubscriptionArn_SetGet;
var
  AList: TksAwsSnsSubscriptionList;
  ASub: IksAwsSnsSubscription;
begin
  AList := TksAwsSnsSubscriptionList.Create;
  try
    ASub := AList.AddSubscription;
    ASub.SubscriptionArn := 'arn:aws:sns:us-east-1:123456789012:MyTopic:abc123';
    Assert.AreEqual('arn:aws:sns:us-east-1:123456789012:MyTopic:abc123', ASub.SubscriptionArn);
  finally
    AList.Free;
  end;
end;

procedure TTestSnsSubscriptionProperties.TestTopicArn_SetGet;
var
  AList: TksAwsSnsSubscriptionList;
  ASub: IksAwsSnsSubscription;
begin
  AList := TksAwsSnsSubscriptionList.Create;
  try
    ASub := AList.AddSubscription;
    ASub.TopicArn := 'arn:aws:sns:us-east-1:123456789012:MyTopic';
    Assert.AreEqual('arn:aws:sns:us-east-1:123456789012:MyTopic', ASub.TopicArn);
  finally
    AList.Free;
  end;
end;

{ TTestSnsSubscriptionLoadFromXml }

procedure TTestSnsSubscriptionLoadFromXml.TestLoadFromXml_AllFields;
var
  AList: TksAwsSnsSubscriptionList;
  ASub: IksAwsSnsSubscription;
begin
  AList := TksAwsSnsSubscriptionList.Create;
  try
    ASub := AList.AddSubscription;
    ASub.LoadFromXML(
      '<member>' +
      '<Endpoint>https://example.com/webhook</Endpoint>' +
      '<Owner>123456789012</Owner>' +
      '<Protocol>https</Protocol>' +
      '<SubscriptionArn>arn:aws:sns:us-east-1:123456789012:MyTopic:sub123</SubscriptionArn>' +
      '<TopicArn>arn:aws:sns:us-east-1:123456789012:MyTopic</TopicArn>' +
      '</member>'
    );
    Assert.AreEqual('https://example.com/webhook', ASub.Endpoint);
    Assert.AreEqual('123456789012', ASub.Owner);
    Assert.AreEqual('https', ASub.Protocol);
    Assert.AreEqual('arn:aws:sns:us-east-1:123456789012:MyTopic:sub123', ASub.SubscriptionArn);
    Assert.AreEqual('arn:aws:sns:us-east-1:123456789012:MyTopic', ASub.TopicArn);
  finally
    AList.Free;
  end;
end;

procedure TTestSnsSubscriptionLoadFromXml.TestLoadFromXml_EmailProtocol;
var
  AList: TksAwsSnsSubscriptionList;
  ASub: IksAwsSnsSubscription;
begin
  AList := TksAwsSnsSubscriptionList.Create;
  try
    ASub := AList.AddSubscription;
    ASub.LoadFromXML(
      '<member>' +
      '<Endpoint>user@example.com</Endpoint>' +
      '<Owner>999888777666</Owner>' +
      '<Protocol>email</Protocol>' +
      '<SubscriptionArn>arn:aws:sns:eu-west-1:999888777666:Alerts:def456</SubscriptionArn>' +
      '<TopicArn>arn:aws:sns:eu-west-1:999888777666:Alerts</TopicArn>' +
      '</member>'
    );
    Assert.AreEqual('user@example.com', ASub.Endpoint);
    Assert.AreEqual('email', ASub.Protocol);
  finally
    AList.Free;
  end;
end;

{ TTestSnsSubscriptionList }

procedure TTestSnsSubscriptionList.TestAddSubscription_ReturnsInterface;
var
  AList: TksAwsSnsSubscriptionList;
  ASub: IksAwsSnsSubscription;
begin
  AList := TksAwsSnsSubscriptionList.Create;
  try
    ASub := AList.AddSubscription;
    Assert.IsNotNull(ASub);
  finally
    AList.Free;
  end;
end;

procedure TTestSnsSubscriptionList.TestAddSubscription_IncreasesCount;
var
  AList: TksAwsSnsSubscriptionList;
begin
  AList := TksAwsSnsSubscriptionList.Create;
  try
    Assert.AreEqual(0, AList.Count);
    AList.AddSubscription;
    Assert.AreEqual(1, AList.Count);
  finally
    AList.Free;
  end;
end;

procedure TTestSnsSubscriptionList.TestAddMultipleSubscriptions;
var
  AList: TksAwsSnsSubscriptionList;
begin
  AList := TksAwsSnsSubscriptionList.Create;
  try
    AList.AddSubscription;
    AList.AddSubscription;
    AList.AddSubscription;
    Assert.AreEqual(3, AList.Count);
  finally
    AList.Free;
  end;
end;

procedure TTestSnsSubscriptionList.TestClearList;
var
  AList: TksAwsSnsSubscriptionList;
begin
  AList := TksAwsSnsSubscriptionList.Create;
  try
    AList.AddSubscription;
    AList.AddSubscription;
    AList.Clear;
    Assert.AreEqual(0, AList.Count);
  finally
    AList.Free;
  end;
end;

{ TTestSnsFactory }

procedure TTestSnsFactory.TestCreateSNS_ReturnsValidInterface;
var
  AService: IksAwsSNS;
begin
  AService := CreateSNS('key', 'secret', awsUsEast1);
  Assert.IsNotNull(AService);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestSnsSubscriptionProperties);
  TDUnitX.RegisterTestFixture(TTestSnsSubscriptionLoadFromXml);
  TDUnitX.RegisterTestFixture(TTestSnsSubscriptionList);
  TDUnitX.RegisterTestFixture(TTestSnsFactory);

end.
