unit TestksAwsHash;

interface

uses
  DUnitX.TestFramework, Classes, SysUtils, IdGlobal;

type
  [TestFixture]
  TTestHashFunctions = class
  public
    [Test]
    procedure TestSHA256_EmptyString;
    [Test]
    procedure TestSHA256_SimpleString;
    [Test]
    procedure TestSHA256_EmptyStream;
    [Test]
    procedure TestSHA256_StreamContent;
    [Test]
    procedure TestSHA256_StreamPositionResetAfterHash;
  end;

  [TestFixture]
  TTestUrlEncoding = class
  public
    [Test]
    procedure TestUrlEncode_PlainString;
    [Test]
    procedure TestUrlEncode_WithSpaces;
    [Test]
    procedure TestParamEncode_PlainString;
    [Test]
    procedure TestParamEncode_WithSpaces;
    [Test]
    procedure TestParamEncode_WithSlash;
    [Test]
    procedure TestParamEncode_WithAmpersand;
    [Test]
    procedure TestParamEncode_WithEquals;
    [Test]
    procedure TestParamEncode_WithSpecialChars;
  end;

  [TestFixture]
  TTestHMACSHA256 = class
  public
    [Test]
    procedure TestCalculateHMACSHA256_KnownVector;
    [Test]
    procedure TestCalculateHMACSHA256Hex_ReturnsHex;
  end;

  [TestFixture]
  TTestGenerateSignature = class
  public
    [Test]
    procedure TestSignature_AwsReferenceVector;
  end;

implementation

uses
  ksAwsHash, ksAwsConst, DateUtils;

{ TTestHashFunctions }

procedure TTestHashFunctions.TestSHA256_EmptyString;
begin
  Assert.AreEqual(C_EMPTY_HASH, GetHashSHA256Hex(''));
end;

procedure TTestHashFunctions.TestSHA256_SimpleString;
begin
  // SHA256('testing') = cf80cd8aed482d5d1527d7dc72fceff84e6326592848447d2dc0b0e87dfc9a90
  Assert.AreEqual('cf80cd8aed482d5d1527d7dc72fceff84e6326592848447d2dc0b0e87dfc9a90',
                  GetHashSHA256Hex('testing'));
end;

procedure TTestHashFunctions.TestSHA256_EmptyStream;
var
  AStream: TStringStream;
begin
  AStream := TStringStream.Create('', TEncoding.UTF8);
  try
    Assert.AreEqual(C_EMPTY_HASH, GetHashSHA256Hex(AStream));
  finally
    AStream.Free;
  end;
end;

procedure TTestHashFunctions.TestSHA256_StreamContent;
var
  AStream: TStringStream;
begin
  AStream := TStringStream.Create('testing', TEncoding.UTF8);
  try
    Assert.AreEqual('cf80cd8aed482d5d1527d7dc72fceff84e6326592848447d2dc0b0e87dfc9a90',
                    GetHashSHA256Hex(AStream));
  finally
    AStream.Free;
  end;
end;

procedure TTestHashFunctions.TestSHA256_StreamPositionResetAfterHash;
var
  AStream: TStringStream;
begin
  AStream := TStringStream.Create('testing', TEncoding.UTF8);
  try
    AStream.Position := 0;
    GetHashSHA256Hex(AStream);
    Assert.AreEqual(Int64(0), Int64(AStream.Position));
  finally
    AStream.Free;
  end;
end;

{ TTestUrlEncoding }

procedure TTestUrlEncoding.TestUrlEncode_PlainString;
begin
  Assert.AreEqual('hello', UrlEncode('hello'));
end;

procedure TTestUrlEncoding.TestUrlEncode_WithSpaces;
var
  AResult: string;
begin
  AResult := UrlEncode('hello world');
  Assert.AreNotEqual('hello world', AResult);
end;

procedure TTestUrlEncoding.TestParamEncode_PlainString;
begin
  Assert.AreEqual('hello', ParamEncode('hello'));
end;

procedure TTestUrlEncoding.TestParamEncode_WithSpaces;
begin
  Assert.AreEqual('hello%20world', ParamEncode('hello world'));
end;

procedure TTestUrlEncoding.TestParamEncode_WithSlash;
var
  AResult: string;
begin
  AResult := ParamEncode('a/b');
  Assert.Contains(AResult, '%2F');
end;

procedure TTestUrlEncoding.TestParamEncode_WithAmpersand;
var
  AResult: string;
begin
  AResult := ParamEncode('a&b');
  Assert.Contains(AResult, '%26');
end;

procedure TTestUrlEncoding.TestParamEncode_WithEquals;
var
  AResult: string;
begin
  AResult := ParamEncode('a=b');
  Assert.Contains(AResult, '%3D');
end;

procedure TTestUrlEncoding.TestParamEncode_WithSpecialChars;
var
  AResult: string;
begin
  AResult := ParamEncode('a:b@c');
  Assert.Contains(AResult, '%3A');
  Assert.Contains(AResult, '%40');
end;

{ TTestHMACSHA256 }

procedure TTestHMACSHA256.TestCalculateHMACSHA256_KnownVector;
var
  AKey: TIdBytes;
  AResult: TIdBytes;
begin
  AKey := IndyTextEncoding_UTF8.GetBytes('secret');
  AResult := CalculateHMACSHA256('message', AKey);
  Assert.IsTrue(Length(AResult) = 32);
end;

procedure TTestHMACSHA256.TestCalculateHMACSHA256Hex_ReturnsHex;
var
  AKey: TIdBytes;
  AResult: string;
begin
  AKey := IndyTextEncoding_UTF8.GetBytes('secret');
  AResult := CalculateHMACSHA256Hex('message', AKey);
  Assert.AreEqual(64, Length(AResult));
end;

{ TTestGenerateSignature }

procedure TTestGenerateSignature.TestSignature_AwsReferenceVector;
var
  ARequestTime: TDateTime;
  AStringToSign: string;
  ASignature: string;
begin
  // AWS Signature Version 4 reference test vector
  // https://docs.aws.amazon.com/general/latest/gr/sigv4-calculate-signature.html
  ARequestTime := EncodeDate(2015, 8, 30);
  AStringToSign := 'AWS4-HMAC-SHA256' + #10 +
                   '20150830T123600Z' + #10 +
                   '20150830/us-east-1/iam/aws4_request' + #10 +
                   'f536975d06c0309214f805bb90ccff089219ecd68b2577efef23edd43b7e1a59';
  ASignature := GenerateSignature(
    ARequestTime,
    AStringToSign,
    'wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY',
    'us-east-1',
    'iam'
  );
  Assert.AreEqual('5d672d79c15b13162d9279b0855cfba6789a8edb4c82c400e06b5924a6f2b5d7',
                  ASignature);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestHashFunctions);
  TDUnitX.RegisterTestFixture(TTestUrlEncoding);
  TDUnitX.RegisterTestFixture(TTestHMACSHA256);
  TDUnitX.RegisterTestFixture(TTestGenerateSignature);

end.
