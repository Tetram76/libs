object FenDBGridPanel1: TFenDBGridPanel1
  Left = 296
  Top = 148
  Width = 513
  Height = 451
  Caption = 'FenDBGridPanel1'
  Color = clBtnFace
  Constraints.MinWidth = 513
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PanelGlobal: TPanel
    Left = 0
    Top = 0
    Width = 505
    Height = 384
    Align = alClient
    BorderWidth = 3
    TabOrder = 0
    object PanelDonnees: TPanel
      Left = 4
      Top = 33
      Width = 497
      Height = 187
      Align = alTop
      BevelOuter = bvLowered
      TabOrder = 1
    end
    object PanelDBGrid: TPanel
      Left = 4
      Top = 245
      Width = 497
      Height = 109
      Align = alClient
      BevelOuter = bvLowered
      BorderWidth = 3
      TabOrder = 4
      object DBGridPrincipal: THDBGrid
        Left = 4
        Top = 4
        Width = 489
        Height = 101
        Align = alClient
        DataSource = DSPrincipal
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = []
        Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit]
        ParentFont = False
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -9
        TitleFont.Name = 'Verdana'
        TitleFont.Style = []
        SpHint = True
      end
    end
    object PanelDBNav: TPanel
      Left = 4
      Top = 354
      Width = 497
      Height = 26
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 3
      object DBNavPrincipal: TDBNavigator
        Left = 0
        Top = 0
        Width = 349
        Height = 26
        DataSource = DSPrincipal
        VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast, nbDelete, nbRefresh]
        Align = alClient
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
      end
      object DBInfoPrincipal: TDBRecordCount
        Left = 349
        Top = 0
        Width = 148
        Height = 26
        DataSource = DSPrincipal
        Align = alRight
        BevelOuter = bvLowered
        TabOrder = 1
        RecordName = 'enregistrement'
        PluralLetter = 's'
      end
    end
    object PanelHaut: TPanel
      Left = 4
      Top = 4
      Width = 497
      Height = 29
      Align = alTop
      BevelInner = bvRaised
      BevelOuter = bvLowered
      TabOrder = 0
    end
    object FindPanelPrincipal: TFindPanel
      Left = 4
      Top = 220
      Width = 497
      Height = 25
      Caption = ' Rechercher '
      VerticalAlignment = vaTop
      VisibleItems = [viButtons, viComboBox, viLabel]
      ChangeIndex = False
      ComboBoxWidth = 120
      Align = alTop
      TabOrder = 2
      TabStop = True
    end
  end
  object PanelBoutons: TPanel
    Left = 0
    Top = 384
    Width = 505
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object DBNavBarrePrincipal: TDBNavBarre
      Left = 0
      Top = 0
      Width = 410
      Height = 40
      Align = alClient
      BevelOuter = bvLowered
      NavMode = nmMultiDatasource
      DataSource = DSPrincipal
      BtnSpacing = 6
      Spacing = 2
      MarginHeight = 3
      MarginWidth = 3
      DeleteMessage = 'Effacer l'#39'enregistrement ?'
    end
    object PanelBoutonFermer: TPanel
      Left = 410
      Top = 0
      Width = 95
      Height = 40
      Align = alRight
      BevelOuter = bvLowered
      TabOrder = 1
      object btnFermer: TBitBtn
        Left = 3
        Top = 3
        Width = 89
        Height = 34
        Caption = '&Fermer'
        TabOrder = 0
        OnClick = btnFermerClick
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
  object DSPrincipal: TDataSource
    Left = 220
    Top = 280
  end
end
