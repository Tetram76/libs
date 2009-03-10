object GLDTSFenAppGenericList: TGLDTSFenAppGenericList
  Left = 108
  Top = 89
  Width = 641
  Height = 460
  Caption = 'Listes G'#233'n'#233'riques'
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 641
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PanelGlobal: TPanel
    Left = 0
    Top = 0
    Width = 633
    Height = 393
    Align = alClient
    BorderWidth = 3
    Caption = 'PanelGlobal'
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 4
      Top = 128
      Width = 625
      Height = 3
      Cursor = crVSplit
      Align = alTop
    end
    object DBGElements: TDBGrid
      Left = 4
      Top = 156
      Width = 625
      Height = 206
      Align = alClient
      DataSource = DSElements
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Verdana'
      TitleFont.Style = []
    end
    object PanelDBNav: TPanel
      Left = 4
      Top = 362
      Width = 625
      Height = 27
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object DBNavPrincipal: TDBNavigator
        Left = 0
        Top = 0
        Width = 490
        Height = 27
        DataSource = DSElements
        Align = alClient
        TabOrder = 0
      end
      object DBInfoPrincipal: TDBRecordCount
        Left = 490
        Top = 0
        Width = 135
        Height = 27
        DataSource = DSElements
        Align = alRight
        BevelOuter = bvLowered
        TabOrder = 1
        RecordName = 'enregistrement'
        PluralLetter = 's'
      end
    end
    object FindPanelPrincipal: TFindPanel
      Left = 4
      Top = 131
      Width = 625
      Height = 25
      AllowCalcFields = False
      Caption = ' Rechercher l'#39#233'l'#233'ment '
      VisibleItems = [viButtons, viComboBox, viLabel]
      SearchField = 'Item'
      ChangeIndex = False
      ComboBoxWidth = 85
      Align = alTop
      TabOrder = 2
      TabStop = True
    end
    object DBGGroupes: TDBGrid
      Left = 4
      Top = 4
      Width = 625
      Height = 124
      Align = alTop
      DataSource = DSGroupes
      Options = [dgEditing, dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete]
      TabOrder = 3
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Verdana'
      TitleFont.Style = []
    end
  end
  object PanelBoutons: TPanel
    Left = 0
    Top = 393
    Width = 633
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object DBNavBarrePrincipal: TDBNavBarre
      Left = 0
      Top = 0
      Width = 538
      Height = 40
      Align = alClient
      BevelOuter = bvLowered
      NavMode = nmMultiDatasource
      DataSource = DSElements
      BtnSpacing = 6
      Spacing = 2
      MarginHeight = 3
      MarginWidth = 3
      DeleteMessage = 'Effacer l'#39'enregistrement ?'
    end
    object PanelBoutonFermer: TPanel
      Left = 538
      Top = 0
      Width = 95
      Height = 40
      Align = alRight
      BevelOuter = bvLowered
      TabOrder = 1
      object FermerBtn: TBitBtn
        Left = 3
        Top = 3
        Width = 89
        Height = 34
        Caption = '&Fermer'
        TabOrder = 0
        OnClick = FermerBtnClick
        Glyph.Data = {
          36060000424D3606000000000000360000002800000020000000100000000100
          1800000000000006000000000000000000000000000000000000007F7F007F7F
          0000000000000000000000000000000000000000000000000000000000000000
          00007F7F007F7F007F7F007F7F007F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F
          7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007F00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7F7F7F7F007F7FFFFFFF007F7F00
          7F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007F00007F00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF7F7F7F007F7FFFFFFF00
          7F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007F00007F00007F00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F7F7F7F007F7FFF
          FFFF007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007F00007F00007F00000000FFFF00FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F7F7F7FFF
          FFFF007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007F00007F00007F00000000FFFF00FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F7F7F7FFF
          FFFF007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007F00007F00007F00000000FFFF00FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F7F7F7FFF
          FFFF007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007F00007F00007F00000000FFFF00FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F7F7F7FFF
          FFFF007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007F00007F00007F00000000FFFF00FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F7F7F7FFF
          FFFF007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007F00007F00007F00000000FFFF00FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F7F7F7FFF
          FFFFFFFFFF007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007F00007F00007F00FFFF00000000FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F7F7F7F7F
          7F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007F00007F00007F00000000FFFF00FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F7F7F7FFF
          FFFF007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007F00007F00007F00000000FFFF00FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7FFFFFFF7F7F7FFF
          FFFF007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007FFFFF0000007F00000000FFFF00FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF7F7F7FFFFFFF7F7F7FFF
          FFFF007F7F007F7F007F7F007F7F7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          00000000007FFFFF00FFFF0000000000FFFF00FFFF00FFFF00FFFF00FFFF0000
          00007F7F007F7F007F7F007F7F007F7F7F7F7FFFFFFF7F7F7F7F7F7F7F7F7FFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF7F7F7FFFFFFF007F7F007F7F007F7F007F7F
          0000000000000000000000000000000000000000000000000000000000000000
          00007F7F007F7F007F7F007F7F007F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F
          7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F007F7F007F7F007F7F}
        NumGlyphs = 2
      end
    end
  end
  object DSElements: TDataSource
    Left = 176
    Top = 100
  end
  object DSGroupes: TDataSource
    Left = 64
    Top = 100
  end
  object DSLienGroupes: TDataSource
    Left = 104
    Top = 60
  end
end
