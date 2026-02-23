unit TestksAwsSes;

interface

uses
  DUnitX.TestFramework, Classes, SysUtils;

type
  [TestFixture]
  TTestSesMessageProperties = class
  public
    [Test]
    procedure TestRecipients_StartsEmpty;
    [Test]
    procedure TestCC_StartsEmpty;
    [Test]
    procedure TestBcc_StartsEmpty;
    [Test]
    procedure TestReplyTo_StartsEmpty;
    [Test]
    procedure TestSender_SetGet;
    [Test]
    procedure TestSubject_SetGet;
    [Test]
    procedure TestBody_SetGet;
    [Test]
    procedure TestHtml_SetGet;
    [Test]
    procedure TestReturnPath_SetGet;
    [Test]
    procedure TestReturnPath_DefaultsEmpty;
    [Test]
    procedure TestAddMultipleRecipients;
    [Test]
    procedure TestAddMultipleCC;
    [Test]
    procedure TestAddMultipleBcc;
    [Test]
    procedure TestAddMultipleReplyTo;
  end;

  [TestFixture]
  TTestSesMessageFactory = class
  public
    [Test]
    procedure TestCreateSesMessage_SingleRecipient;
    [Test]
    procedure TestCreateSesMessage_SetsSender;
    [Test]
    procedure TestCreateSesMessage_SetsSubject;
    [Test]
    procedure TestCreateSesMessage_SetsBody;
    [Test]
    procedure TestCreateSesMessage_HtmlDefaultsEmpty;
    [Test]
    procedure TestCreateSesMessage_ReturnPathDefaultsEmpty;
    [Test]
    procedure TestCreateSesMessage_MultipleRecipients;
    [Test]
    procedure TestCreateSesMessage_MultipleRecipients_Count;
  end;

  [TestFixture]
  TTestSesServiceFactory = class
  public
    [Test]
    procedure TestCreateSes_ReturnsValidInterface;
  end;

implementation

uses
  ksAwsBase, ksAwsSes;

{ TTestSesMessageProperties }

procedure TTestSesMessageProperties.TestRecipients_StartsEmpty;
var
  AMsg: IksAwsSesMessage;
begin
  AMsg := CreateSesMessage('', '', '', '');
  AMsg.Recipients.Clear;
  Assert.AreEqual(0, AMsg.Recipients.Count);
end;

procedure TTestSesMessageProperties.TestCC_StartsEmpty;
var
  AMsg: IksAwsSesMessage;
begin
  AMsg := CreateSesMessage('to@test.com', 'from@test.com', 'subj', 'body');
  Assert.AreEqual(0, AMsg.CC.Count);
end;

procedure TTestSesMessageProperties.TestBcc_StartsEmpty;
var
  AMsg: IksAwsSesMessage;
begin
  AMsg := CreateSesMessage('to@test.com', 'from@test.com', 'subj', 'body');
  Assert.AreEqual(0, AMsg.Bcc.Count);
end;

procedure TTestSesMessageProperties.TestReplyTo_StartsEmpty;
var
  AMsg: IksAwsSesMessage;
begin
  AMsg := CreateSesMessage('to@test.com', 'from@test.com', 'subj', 'body');
  Assert.AreEqual(0, AMsg.ReplyTo.Count);
end;

procedure TTestSesMessageProperties.TestSender_SetGet;
var
  AMsg: IksAwsSesMessage;
begin
  AMsg := CreateSesMessage('to@test.com', 'from@test.com', 'subj', 'body');
  AMsg.Sender := 'new@sender.com';
  Assert.AreEqual('new@sender.com', AMsg.Sender);
end;

procedure TTestSesMessageProperties.TestSubject_SetGet;
var
  AMsg: IksAwsSesMessage;
begin
  AMsg := CreateSesMessage('to@test.com', 'from@test.com', 'subj', 'body');
  AMsg.Subject := 'New Subject';
  Assert.AreEqual('New Subject', AMsg.Subject);
end;

procedure TTestSesMessageProperties.TestBody_SetGet;
var
  AMsg: IksAwsSesMessage;
begin
  AMsg := CreateSesMessage('to@test.com', 'from@test.com', 'subj', 'body');
  AMsg.Body := 'New Body';
  Assert.AreEqual('New Body', AMsg.Body);
end;

procedure TTestSesMessageProperties.TestHtml_SetGet;
var
  AMsg: IksAwsSesMessage;
begin
  AMsg := CreateSesMessage('to@test.com', 'from@test.com', 'subj', 'body');
  AMsg.Html := '<h1>Hello</h1>';
  Assert.AreEqual('<h1>Hello</h1>', AMsg.Html);
end;

procedure TTestSesMessageProperties.TestReturnPath_SetGet;
var
  AMsg: IksAwsSesMessage;
begin
  AMsg := CreateSesMessage('to@test.com', 'from@test.com', 'subj', 'body');
  AMsg.ReturnPath := 'bounce@test.com';
  Assert.AreEqual('bounce@test.com', AMsg.ReturnPath);
end;

procedure TTestSesMessageProperties.TestReturnPath_DefaultsEmpty;
var
  AMsg: IksAwsSesMessage;
begin
  AMsg := CreateSesMessage('to@test.com', 'from@test.com', 'subj', 'body');
  Assert.AreEqual('', AMsg.ReturnPath);
end;

procedure TTestSesMessageProperties.TestAddMultipleRecipients;
var
  AMsg: IksAwsSesMessage;
begin
  AMsg := CreateSesMessage('first@test.com', 'from@test.com', 'subj', 'body');
  AMsg.Recipients.Add('second@test.com');
  AMsg.Recipients.Add('third@test.com');
  Assert.AreEqual(3, AMsg.Recipients.Count);
end;

procedure TTestSesMessageProperties.TestAddMultipleCC;
var
  AMsg: IksAwsSesMessage;
begin
  AMsg := CreateSesMessage('to@test.com', 'from@test.com', 'subj', 'body');
  AMsg.CC.Add('cc1@test.com');
  AMsg.CC.Add('cc2@test.com');
  Assert.AreEqual(2, AMsg.CC.Count);
end;

procedure TTestSesMessageProperties.TestAddMultipleBcc;
var
  AMsg: IksAwsSesMessage;
begin
  AMsg := CreateSesMessage('to@test.com', 'from@test.com', 'subj', 'body');
  AMsg.Bcc.Add('bcc1@test.com');
  AMsg.Bcc.Add('bcc2@test.com');
  Assert.AreEqual(2, AMsg.Bcc.Count);
end;

procedure TTestSesMessageProperties.TestAddMultipleReplyTo;
var
  AMsg: IksAwsSesMessage;
begin
  AMsg := CreateSesMessage('to@test.com', 'from@test.com', 'subj', 'body');
  AMsg.ReplyTo.Add('reply1@test.com');
  AMsg.ReplyTo.Add('reply2@test.com');
  Assert.AreEqual(2, AMsg.ReplyTo.Count);
end;

{ TTestSesMessageFactory }

procedure TTestSesMessageFactory.TestCreateSesMessage_SingleRecipient;
var
  AMsg: IksAwsSesMessage;
begin
  AMsg := CreateSesMessage('to@test.com', 'from@test.com', 'Test Subject', 'Test Body');
  Assert.AreEqual(1, AMsg.Recipients.Count);
  Assert.AreEqual('to@test.com', AMsg.Recipients[0]);
end;

procedure TTestSesMessageFactory.TestCreateSesMessage_SetsSender;
var
  AMsg: IksAwsSesMessage;
begin
  AMsg := CreateSesMessage('to@test.com', 'from@test.com', 'subj', 'body');
  Assert.AreEqual('from@test.com', AMsg.Sender);
end;

procedure TTestSesMessageFactory.TestCreateSesMessage_SetsSubject;
var
  AMsg: IksAwsSesMessage;
begin
  AMsg := CreateSesMessage('to@test.com', 'from@test.com', 'My Subject', 'body');
  Assert.AreEqual('My Subject', AMsg.Subject);
end;

procedure TTestSesMessageFactory.TestCreateSesMessage_SetsBody;
var
  AMsg: IksAwsSesMessage;
begin
  AMsg := CreateSesMessage('to@test.com', 'from@test.com', 'subj', 'My Body');
  Assert.AreEqual('My Body', AMsg.Body);
end;

procedure TTestSesMessageFactory.TestCreateSesMessage_HtmlDefaultsEmpty;
var
  AMsg: IksAwsSesMessage;
begin
  AMsg := CreateSesMessage('to@test.com', 'from@test.com', 'subj', 'body');
  Assert.AreEqual('', AMsg.Html);
end;

procedure TTestSesMessageFactory.TestCreateSesMessage_ReturnPathDefaultsEmpty;
var
  AMsg: IksAwsSesMessage;
begin
  AMsg := CreateSesMessage('to@test.com', 'from@test.com', 'subj', 'body');
  Assert.AreEqual('', AMsg.ReturnPath);
end;

procedure TTestSesMessageFactory.TestCreateSesMessage_MultipleRecipients;
var
  AMsg: IksAwsSesMessage;
  ARecipients: TStringList;
begin
  ARecipients := TStringList.Create;
  try
    ARecipients.Add('one@test.com');
    ARecipients.Add('two@test.com');
    ARecipients.Add('three@test.com');
    AMsg := CreateSesMessage(ARecipients, 'from@test.com', 'subj', 'body');
    Assert.AreEqual(3, AMsg.Recipients.Count);
    Assert.AreEqual('one@test.com', AMsg.Recipients[0]);
    Assert.AreEqual('two@test.com', AMsg.Recipients[1]);
    Assert.AreEqual('three@test.com', AMsg.Recipients[2]);
  finally
    ARecipients.Free;
  end;
end;

procedure TTestSesMessageFactory.TestCreateSesMessage_MultipleRecipients_Count;
var
  AMsg: IksAwsSesMessage;
  ARecipients: TStringList;
begin
  ARecipients := TStringList.Create;
  try
    ARecipients.Add('a@test.com');
    ARecipients.Add('b@test.com');
    AMsg := CreateSesMessage(ARecipients, 'from@test.com', 'subj', 'body');
    Assert.AreEqual(2, AMsg.Recipients.Count);
  finally
    ARecipients.Free;
  end;
end;

{ TTestSesServiceFactory }

procedure TTestSesServiceFactory.TestCreateSes_ReturnsValidInterface;
var
  AService: IksAwsSES;
begin
  AService := CreateSes('key', 'secret', awsEuWest1);
  Assert.IsNotNull(AService);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestSesMessageProperties);
  TDUnitX.RegisterTestFixture(TTestSesMessageFactory);
  TDUnitX.RegisterTestFixture(TTestSesServiceFactory);

end.
