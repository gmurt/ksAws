object Form10: TForm10
  Left = 0
  Top = 0
  Caption = 'Form10'
  ClientHeight = 306
  ClientWidth = 250
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lbSenders: TListBox
    Left = 0
    Top = 25
    Width = 250
    Height = 281
    Align = alClient
    ItemHeight = 13
    TabOrder = 0
  end
  object btnGetVerifiedSenders: TButton
    Left = 0
    Top = 0
    Width = 250
    Height = 25
    Align = alTop
    Caption = 'List Verified Senders'
    TabOrder = 1
    OnClick = btnGetVerifiedSendersClick
  end
  object IdSSLIOHandlerSocketOpenSSL1: TIdSSLIOHandlerSocketOpenSSL
    MaxLineAction = maException
    Port = 0
    DefaultPort = 0
    SSLOptions.Mode = sslmUnassigned
    SSLOptions.VerifyMode = []
    SSLOptions.VerifyDepth = 0
    Left = 136
    Top = 104
  end
end
