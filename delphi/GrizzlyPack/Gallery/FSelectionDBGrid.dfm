object FenSelDBGrid1: TFenSelDBGrid1
  Left = 282
  Top = 150
  ActiveControl = FindPanelPrincipal
  BorderStyle = bsDialog
  Caption = 'FenSelDBGrid1'
  ClientHeight = 373
  ClientWidth = 521
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PanelGlobal: TPanel
    Left = 0
    Top = 0
    Width = 521
    Height = 293
    Align = alClient
    BorderWidth = 3
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object DBGridPrincipal: THDBGrid
      Left = 4
      Top = 58
      Width = 513
      Height = 206
      Align = alClient
      Constraints.MinWidth = 513
      DataSource = DSPrincipal
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = []
      Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgConfirmDelete]
      ParentFont = False
      ReadOnly = True
      TabOrder = 1
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -9
      TitleFont.Name = 'Verdana'
      TitleFont.Style = []
      OnDblClick = DBGridPrincipalDblClick
      SpHint = True
    end
    object PanelDBNavDBInfo: TPanel
      Left = 4
      Top = 264
      Width = 513
      Height = 25
      Align = alBottom
      BevelOuter = bvNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      object DBNavPrincipal: TDBNavigator
        Left = 0
        Top = 0
        Width = 358
        Height = 25
        DataSource = DSPrincipal
        VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast, nbRefresh]
        Align = alClient
        TabOrder = 0
      end
      object DBInfoPrincipal: TDBRecordCount
        Left = 358
        Top = 0
        Width = 155
        Height = 25
        DataSource = DSPrincipal
        Align = alRight
        BevelOuter = bvLowered
        TabOrder = 1
        RecordName = 'enregistrement'
        PluralLetter = 's'
      end
    end
    object FindPanelPrincipal: TFindPanel
      Left = 4
      Top = 33
      Width = 513
      Height = 25
      Caption = ' Rechercher '
      VisibleItems = [viButtons, viComboBox, viLabel]
      ChangeIndex = False
      ComboBoxWidth = 120
      Align = alTop
      BevelOuter = bvLowered
      TabOrder = 0
      TabStop = True
    end
    object PanelHaut: TPanel
      Left = 4
      Top = 4
      Width = 513
      Height = 29
      Align = alTop
      BevelOuter = bvLowered
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
    end
  end
  object ExitPanel1: TExitPanel
    Left = 0
    Top = 333
    Width = 521
    Height = 40
    BtnHeight = 33
    BtnWidth = 94
    BtnOk.Caption = 'OK'
    BtnOk.Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333330000333333333333333333333333F33333333333
      00003333344333333333333333388F3333333333000033334224333333333333
      338338F3333333330000333422224333333333333833338F3333333300003342
      222224333333333383333338F3333333000034222A22224333333338F338F333
      8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
      33333338F83338F338F33333000033A33333A222433333338333338F338F3333
      0000333333333A222433333333333338F338F33300003333333333A222433333
      333333338F338F33000033333333333A222433333333333338F338F300003333
      33333333A222433333333333338F338F00003333333333333A22433333333333
      3338F38F000033333333333333A223333333333333338F830000333333333333
      333A333333333333333338330000333333333333333333333333333333333333
      0000}
    BtnOk.ModalResult = 1
    BtnOk.Cancel = False
    BtnOk.Default = True
    BtnOk.Kind = bkOK
    BtnCancel.Caption = 'Annuler'
    BtnCancel.Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      333333333333333333333333060033338833333333333333333F333333333333
      B901333911833333983333333388F333333F33335A0233391118333911833333
      38F38F333F88F33302003339111183911118333338F338F3F8338F33AB013333
      911118111118333338F3338F833338F3020033333911111111833333338F3338
      3333F833B681333333911111183333333338F333333F83331900333333311111
      8333333333338F3333383333600233333339111183333333333338F333833333
      60023333339111118333333333333833338F3333030033333911181118333333
      33338333338F333360023333911183911183333333383338F338F33302003333
      9118333911183333338F33838F338F3300A033333913333391113333338FF833
      38F338F35A023333333333333919333333388333338FFF830200333333333333
      333333333333333333388833B581333333333333333333333333333333333333
      B581}
    BtnCancel.ModalResult = 2
    BtnCancel.Cancel = True
    BtnCancel.Default = False
    BtnCancel.Kind = bkCancel
    Spacing = 6
    Align = alBottom
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentFont = False
  end
  object DBNavBarrePrincipal: TDBNavBarre
    Left = 0
    Top = 293
    Width = 521
    Height = 40
    Align = alBottom
    BevelOuter = bvLowered
    NavMode = nmSingleDatasource
    DataSource = DSPrincipal
    BtnSpacing = 6
    Spacing = 2
    MarginHeight = 3
    MarginWidth = 3
    DeleteMessage = 'Effacer l'#39'enregistrement ?'
  end
  object DSPrincipal: TDataSource
    Left = 316
    Top = 156
  end
end
