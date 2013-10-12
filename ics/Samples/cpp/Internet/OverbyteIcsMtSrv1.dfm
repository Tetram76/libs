object ServerForm: TServerForm
  Left = 505
  Top = 256
  Caption = 'ServerForm'
  ClientHeight = 239
  ClientWidth = 264
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object DisconnectButton: TButton
    Left = 16
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Disconnect'
    TabOrder = 0
    OnClick = DisconnectButtonClick
  end
  object DisconnectAllButton: TButton
    Left = 96
    Top = 8
    Width = 75
    Height = 25
    Caption = 'DisconnectAll'
    TabOrder = 1
    OnClick = DisconnectAllButtonClick
  end
  object QuitButton: TButton
    Left = 176
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Quit'
    TabOrder = 2
    OnClick = QuitButtonClick
  end
  object ClientListBox: TListBox
    Left = 16
    Top = 40
    Width = 153
    Height = 193
    ItemHeight = 13
    TabOrder = 3
  end
  object ServerWSocket: TWSocket
    LineMode = False
    LineLimit = 65536
    LineEnd = #13#10
    LineEcho = False
    LineEdit = False
    Proto = 'tcp'
    LocalAddr = '0.0.0.0'
    LocalPort = '0'
    MultiThreaded = False
    MultiCast = False
    MultiCastIpTTL = 1
    FlushTimeout = 60
    SendFlags = wsSendNormal
    LingerOnOff = wsLingerOn
    LingerTimeout = 0
    KeepAliveOnOff = wsKeepAliveOff
    KeepAliveTime = 0
    KeepAliveInterval = 0
    SocksLevel = '5'
    SocksAuthentication = socksNoAuthentication
    LastError = 0
    ReuseAddr = False
    ComponentOptions = []
    ListenBacklog = 5
    ReqVerLow = 2
    ReqVerHigh = 2
    OnSessionAvailable = ServerWSocketSessionAvailable
    Left = 192
    Top = 120
  end
end
