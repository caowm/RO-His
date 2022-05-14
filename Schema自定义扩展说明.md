# Schema自定义扩展说明

DataAbstract Schema是数据服务的核心，Schema里面提供了CustomAttributes自定义属性配置，我们正是在这里面加入了很多属性来实现一些非常繁琐而又实用的功能。这些功能主要是为了方便客户端开发，而同样可以实现服务端功能开发，比如在CustomAttributes配置role属性，实现服务端RBAC。目前RBAC只是在客户端上实现了，而后台是没有实现的，后台只有Session控制，Session只在用户登录时创建，用户退出时销毁。

Schema CustomAttributes主要用于各种基础视图的创建，包括表单窗体(TFormDataView)，表格视图(TTableGridDataView)，Card视图(TCardGridDataView)，Tree视图(TTreeDataView)等等，基础视图定义核心文件：App_DAView.pas和App_DevExpress.pas。

创建视图的时候，我们最繁重的工作是根据业务数据来创建视图的内容，而Schema DataTable的CustomAttributes就是我们加进去的业务视图信息。我们看看表格视图在设置数据时的代码：
```delphi
procedure TTableGridDataView.SetData(Data: TCustomData);
begin
  TableView.DataController.DataSource := nil;
  TableView.ClearItems;
  inherited;

  if Data = nil then
    Exit;
  { 重新建立视图 }
  DevExpress.BuildTableView(TableView, FCustomData.Source);
  BuildEditButtonEvent;
  BuildFilterFields;
  RestoreViewLayout(AppCore.UserIni);

  ConfigViewAfterData;
end;


procedure TDevExpressModule.BuildTableView(TableView: TcxGridDBTableView;
  DataSource: TDADataSource);
var
  Value: string;
  Attr: TStrings;
  DataTable: TDADataTable;
begin
  Assert(DataSource <> nil);
  Assert(DataSource.DataTable <> nil);

  // RegisterGridStyleListener(TableView);

  // 确保DataTable的Schema可用
  DataTable := DataSource.DataTable;
  Attr := DataTable.CustomAttributes;
  with TableView do
  begin
    BeginUpdate;
    try
      OnMouseWheel := DoTableViewMouseWheel;
      ClearItems;
      DataController.Summary.DefaultGroupSummaryItems.Clear;
      // FilterBox.Visible := fvNonEmpty;

      // 不区分大小写对筛选性能有一点影响
      DataController.Filter.Options := DataController.Filter.Options +
        [fcoCaseInsensitive];
      OptionsCustomize.ColumnHidingOnGrouping := False;
      OptionsCustomize.ColumnsQuickCustomization := True;
      OptionsCustomize.DataRowSizing := True;
      OptionsCustomize.ColumnFiltering := True;
      OptionsData.Deleting := False;
      OptionsData.Inserting := False;
      OptionsData.Editing := DataSource.AutoEdit;

      // OptionsSelection.MultiSelect := True;
      // OptionsSelection.HideSelection := False;
      // OptionsSelection.InvertSelect := True;

      OptionsView.GroupByBox := True;
      OptionsView.GroupSummaryLayout := gslAlignWithColumnsAndDistribute;
      OptionsView.Indicator := True;
      OptionsView.IndicatorWidth := iIndicatorWidth;
      OptionsView.DataRowHeight := iDefaultDataRowHeight;
      // OptionsView.GroupFooters := gfVisibleWhenExpanded;
      // OptionsView.ShowEditButtons := gsebForFocusedRecord;
      // OptionsView.Navigator := False;
      // OptionsView.HeaderHeight := iDefaultDataRowHeight;
      // OptionsView.GroupRowHeight := iDefaultDataRowHeight;

      OptionsBehavior.NavigatorHints := True;
      OptionsBehavior.GoToNextCellOnEnter := True;
      OptionsBehavior.FocusCellOnCycle := True;
      OptionsBehavior.FocusFirstCellOnNewRecord := True;
      OptionsBehavior.BestFitMaxRecordCount := 50;

      DataController.DataSource := DataSource;
      OnCustomDrawIndicatorCell := cxTableViewCustomDrawIndicatorCell;
      Styles := GetGridStyle;

      DataController.CreateAllItems;
      DataController.KeyFieldNames := Attr.Values['KeyFieldNames'];

      Value := Attr.Values['OptionsData.Editing'];
      if Value <> '' then
        OptionsData.Editing := StrToBoolDef(Value, False);

      Value := Attr.Values['OptionsData.Deleting'];
      if Value <> '' then
        OptionsData.Deleting := StrToBoolDef(Value, False);

      Value := Attr.Values['OptionsData.Inserting'];
      if Value <> '' then
        OptionsData.Inserting := StrToBoolDef(Value, False);

      Value := Attr.Values['OptionsView.GroupByBox'];
      if Value <> '' then
        OptionsView.GroupByBox := StrToBoolDef(Value, False);

      Value := Attr.Values['OptionsView.ShowEditButtons'];
      if Value <> '' then
        OptionsView.ShowEditButtons := TcxGridShowEditButtons
          (GetEnumValue(TypeInfo(TcxGridShowEditButtons), Value));

      Value := Attr.Values['OptionsView.IndicatorWidth'];
      if Value <> '' then
        OptionsView.IndicatorWidth := StrToIntDef(Value, iIndicatorWidth);

      Value := Attr.Values['OptionsView.DataRowHeight'];
      if Value <> '' then
        OptionsView.DataRowHeight := StrToIntDef(Value, iDefaultDataRowHeight);

      Value := Attr.Values['OptionsView.CellAutoHeight'];
      if Value <> '' then
        OptionsView.CellAutoHeight := StrToBoolDef(Value, False);

      Value := Attr.Values['OptionsView.Footer'];
      if Value <> '' then
        OptionsView.Footer := StrToBoolDef(Value, False);

      Value := Attr.Values['OptionsSelection.MultiSelect'];
      if Value <> '' then
        OptionsSelection.MultiSelect := StrToBoolDef(Value, False);

      Value := Attr.Values['OptionsCustomize.ColumnSorting'];
      if Value <> '' then
        OptionsCustomize.ColumnSorting := StrToBoolDef(Value, True);

      Value := Attr.Values['BackgroundBitmaps'];
      if Value <> '' then
        BackgroundBitmaps.Background.Assign(AppCore.ImageCenter.Get(Value));

      // Value := Attr.Values['Preview.Column'];
      // if Value <> '' then
      // begin
      // Preview.Visible := True;
      // Preview.Column := GetColumnByFieldName(Value);
      // end;

      BuildGridColumns(TableView, DataSource);

      if Assigned(FOnBuildTableView) then
        FOnBuildTableView(TableView);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TDevExpressModule.BuildGridColumns(TableView: TcxGridDBTableView;
  DataSource: TDADataSource);
var
  I: Integer;
  Column: TcxGridDBColumn;
  DataTable: TDADataTable;
begin
  DataTable := DataSource.DataTable;
  for I := 0 to DataTable.FieldCount - 1 do
  begin
    Column := TableView.GetColumnByFieldName(DataTable.Fields[I].Name);
    if Column <> nil then
      BuildGridColumn(Column, DataTable.Fields[I], DataSource);
  end;
end;


procedure TDevExpressModule.BuildGridColumn(Column: TcxGridColumn;
  DAField: TDAField; DataSource: TDADataSource);
var
  Attr: TStrings;
  Value: string;
  CommonPropertiesCount: Integer;
begin
  Assert(Column <> nil);
  Assert(DAField <> nil);

  try
    Attr := DAField.CustomAttributes;

    Column.HeaderAlignmentHorz := taCenter;
    Column.HeaderAlignmentVert := vaCenter;

    if DAField.DisplayWidth <= 0 then
    begin
      if DAField.DataType in [datDateTime, datFloat, datCurrency, datAutoInc,
        datInteger, datLargeInt, datBoolean, datDecimal, datCardinal,
        datLargeAutoInc, datByte, datShortInt, datWord, datSmallInt,
        datLargeUInt, datSingleFloat] then
        Column.Width := FNumColumnWidth
      else
        Column.Width := FTextColumnWidth
    end;

    // 优化速度! 注意这里!!! 小心代码被屏蔽!!!
    CommonPropertiesCount := Attr.Count;
    if CommonPropertiesCount = 0 then
      Exit;

    Value := Attr.Values['MultiEditorField'];
    if Value <> '' then
    begin
      Column.Tag := Integer(TcxGridDBTableView(Column.GridView)
        .GetColumnByFieldName(Value));
      if Column.Tag <> 0 then
        Column.OnGetPropertiesForEdit := DoTableViewGetProperties;
      Exit;
    end;

    Value := Attr.Values['Properties'];
    Column.PropertiesClass := GetPropertiesClass(Value);
    Column.Properties := GetProperties(Value);
    if Value <> '' then
    begin
      Dec(CommonPropertiesCount);
    end;

    Value := Attr.Values['Options.Editing'];
    if Value <> '' then
    begin
      Column.Options.Editing := StrToBoolDef(Value, False);
      Column.Options.Focusing := Column.Options.Editing;
      Dec(CommonPropertiesCount);
    end;

    BuildProperties(Column, Column.Properties, DAField, DataSource);

    // 优化速度! 注意这里!!! 小心代码被屏蔽!!!
    if CommonPropertiesCount <= 0 then
      Exit;

    Value := Attr.Values['Options.Focusing'];
    if Value <> '' then
    begin
      Column.Options.Focusing := StrToBoolDef(Value, True);
    end;

    Value := Attr.Values['Styles.Content'];
    if Value <> '' then
    begin
      Column.Styles.Content := TcxStyle(GetStyle(Value));
    end;

    Value := Attr.Values['Styles.Footer'];
    if Value <> '' then
    begin
      Column.Styles.Footer := TcxStyle(GetStyle(Value));
    end;

    {
      Value := Attr.Values['Options.CellMerging'];
      if Value <> '' then
      begin
      Column.Options.CellMerging := StrToBoolDef(Value, False);
      end;

      Value := Attr.Values['Options.Focusing'];
      if Value <> '' then
      Column.Options.Focusing := StrToBoolDef(Value, False);
    }

    Value := Attr.Values['GroupIndex'];
    if Value <> '' then
      Column.GroupIndex := StrToIntDef(Value, 0);

    // TcxSummaryKind = (skNone, skSum, skMin, skMax, skCount, skAverage);
    Value := Attr.Values['Summary.FooterKind'];
    if Value <> '' then
      Column.Summary.FooterKind :=
        TcxSummaryKind(GetEnumValue(TypeInfo(TcxSummaryKind), Value));

    Value := Attr.Values['Summary.FooterFormat'];
    if Value <> '' then
      Column.Summary.FooterFormat := Value;

    Value := Attr.Values['Summary.GroupFooterKind'];
    if Value <> '' then
    begin
      Column.Summary.GroupFooterKind :=
        TcxSummaryKind(GetEnumValue(TypeInfo(TcxSummaryKind), Value));
      Column.Summary.GroupKind := Column.Summary.GroupFooterKind;
    end;

    Value := Attr.Values['Summary.GroupFooterFormat'];
    if Value <> '' then
    begin
      Column.Summary.GroupFooterFormat := Value;
      Column.Summary.GroupFormat := Value
    end;

  except
    on E: Exception do
      AppCore.Logger.Write('(BuildGridColumn)' + E.Message, mtError, 0)
  end;
end;
```
上面的代码自动帮我们设置好表头和表尾，以及每列的编辑属性。看代码我们可以知道在DataTable CustomAttributes可以设置哪些属性，Fields CustomAttribute可以设置哪些属性。

再来看看表单视图在设置数据时的代码：
```delphi
procedure TFormDataView.SetData(ACustomData: TCustomData);
begin
  inherited;
  ToolBarGroup.Visible := StrToBoolDef(ACustomData.CustomAttributes.Values
    ['FormToolBar'], True);
  ImageName := ACustomData.CustomAttributes.Values['FormImageName'];
  BuildDataEditor;
end;

procedure TFormDataView.BuildDataEditor;
begin
  { 根据Data创建表单 }
  ViewLayout.BeginUpdate;
  try
    FreeAndNil(FEditorContainer);
    if ViewData = nil then
      Exit;
    FEditorContainer := FEditorGroup.CreateGroup();
    FEditorContainer.ShowBorder := False;
    DevExpress.BuildFormView(FEditorContainer, ViewData);
  finally
    ViewLayout.EndUpdate;
  end;
end;

{ 选取可见的字段进行表单创建 }

procedure TDevExpressModule.BuildFormView(AGroup: TdxlayoutGroup;
  ACustomData: TCustomData);
var
  I, J: Integer;
  VisibleFields: TStringDynArray;
  FormEditFields: string;
  FormControlWidth: Integer;
  CaptionAlignTop: Boolean;
begin

  with ACustomData.Table do
  begin
    FormEditFields := CustomAttributes.Values['FormEditFields'];
    FormControlWidth :=
      StrToIntDef(CustomAttributes.Values['FormControlWidth'], 0);
    CaptionAlignTop := False;

    if FormEditFields <> '' then
      VisibleFields := DelimitedTextToArray(FormEditFields)
    else
    begin
      // 分配足够的空间
      SetLength(VisibleFields, FieldCount);
      J := 0;
      for I := 0 to FieldCount - 1 do
      begin
        if Fields[I].Visible then
        begin
          VisibleFields[J] := Fields[I].Name;
          Inc(J);
        end;
      end;
      // 分配实际的空间
      SetLength(VisibleFields, J);
    end;
  end;
  BuildFormView2(AGroup, ACustomData.Source, VisibleFields, FormControlWidth,
    CaptionAlignTop);
end;

{ 根据选取字段进行表单创建 }

procedure TDevExpressModule.BuildFormView2(AGroup: TdxlayoutGroup;
  ADataSource: TDADataSource; AFields: array of string; AControlWidth: Integer;
  ACaptionAlignTop: Boolean);
var
  I: Integer;
  Editor: TcxCustomEdit;
  BandGroup: TdxlayoutGroup;
  EditorGroup: TdxlayoutGroup;
  Value: Variant;
  DefaultBeginsLayer: Boolean;
  DAField: TDAField;
begin
  EditorGroup := nil;

  DefaultBeginsLayer :=
    StrToBoolDef(ADataSource.DataTable.CustomAttributes.Values
    ['DefaultBeginsLayer'], True);
  if AControlWidth <= 0 then
    AControlWidth := StrToIntDef(ADataSource.DataTable.CustomAttributes.Values
      ['FormControlWidth'], 0);

  for I := 0 to Length(AFields) - 1 do
  begin
    DAField := ADataSource.DataTable.FieldByName(AFields[I]);

    Value := DAField.CustomAttributes.Values['BandCaption'];
    if Value <> '' then
    begin
      BandGroup := AGroup.CreateGroup();
      BandGroup.Caption := Value;
      // BandGroup.ButtonOptions.ShowExpandButton := True;
      EditorGroup := nil;
    end;

    Value := DAField.CustomAttributes.Values['BeginsLayer'];
    if (EditorGroup = nil) or StrToBoolDef(Value, DefaultBeginsLayer) then
    begin
      if BandGroup <> nil then
        EditorGroup := BandGroup.CreateGroup()
      else
        EditorGroup := AGroup.CreateGroup();

      EditorGroup.ShowBorder := False;
      EditorGroup.LayoutDirection := ldHorizontal;
      // EditorGroup.ItemControlAreaAlignment := catAuto;
    end;

    Editor := BuildFormEditor(ADataSource, DAField, AGroup, AControlWidth);

    with EditorGroup.CreateItemForControl(Editor) do
    begin
      Caption := DAField.DisplayLabel + ':';
      if StrToBoolDef(DAField.CustomAttributes.Values['AlignHorz'], False) then
        AlignHorz := ahClient;
      AlignVert := avCenter;
      // CaptionOptions.AlignVert := tavTop;
      if ACaptionAlignTop then
        CaptionOptions.Layout := clTop;
    end;
    // if Assigned(FOnWrapControl) then FOnWrapControl(Editor, Fields[I]);
  end;
end;


function TDevExpressModule.BuildFormEditor(ADataSource: TDADataSource;
  ADAField: TDAField; AOwner: TComponent; AEditorWidth: Integer): TcxCustomEdit;
var
  Att: string;
  CommonPropertiesCount: Integer;
begin
  CommonPropertiesCount := ADAField.CustomAttributes.Count;

  Att := ADAField.CustomAttributes.Values['Properties'];
  if Att <> '' then
    Dec(CommonPropertiesCount);

  Result := GetDBControlClass(Att).Create(AOwner);
  with TcxDBEditDataBinding(TcxCustomEditAccess(Result).DataBinding) do
  begin
    DataSource := ADataSource;
    DataField := ADAField.Name;
  end;

  // 设置控件宽度
  Att := ADAField.CustomAttributes.Values['ControlWidth'];
  if Att <> '' then
    Result.Width := StrToIntDef(Att, Round(TextColumnWidth * 1.8))
  else
    Result.Width := IfThen(AEditorWidth <= 0, Round(TextColumnWidth * 1.8),
      AEditorWidth);

  if Result.InheritsFrom(TcxCustomCheckBox) then
    TcxCustomCheckBox(Result).Transparent := True;

  Att := ADAField.CustomAttributes.Values['Options.Editing'];
  if Att <> '' then
  begin
    Result.TabStop := StrToBoolDef(Att, False);
    Dec(CommonPropertiesCount);
  end;

  BuildProperties(Result, Result.ActiveProperties, ADAField, ADataSource);

  // 优化速度! 注意这里!!! 小心代码被屏蔽!!!
  if CommonPropertiesCount <= 0 then
    Exit;

  Att := ADAField.CustomAttributes.Values['Options.Focusing'];
  if Att <> '' then
  begin
    Result.TabStop := StrToBoolDef(Att, True);
  end;

  Att := ADAField.CustomAttributes.Values['Style.Color'];
  if Att <> '' then
  begin
    Result.Style.Color := StringToColor(Att);
  end;

  Att := ADAField.CustomAttributes.Values['Style.TextColor'];
  if Att <> '' then
  begin
    Result.Style.TextColor := StringToColor(Att);
  end;

  Att := ADAField.CustomAttributes.Values['Style.TextStyle.fsBold'];
  if (Att <> '') and StrToBoolDef(Att, False) then
  begin
    Result.Style.TextStyle := Result.Style.TextStyle + [fsBold];
  end;

  Att := ADAField.CustomAttributes.Values['Style.Font.Size'];
  if (Att <> '') then
  begin
    Result.Style.Font.Size := StrToIntDef(Att, 14);
  end;

  Att := ADAField.CustomAttributes.Values['Style.Font.Name'];
  if (Att <> '') then
  begin
    Result.Style.Font.Name := Att;
  end;
end;

```
这些重要的基础视图的创建都是基于DevExpress强大的控件，窗体布局基于DevExpress Layout。这些控件既美观又好用，还能减少代码量。

这些视图的创建最终都将调用下面两个重要的函数，用于完成各种控件properties的设定：

```delphi
procedure TDevExpressModule.BuildProperties(AColumn: TComponent;
  EditProperties: TcxCustomEditProperties; DAField: TDAField;
  DataSource: TDADataSource);
begin
  if EditProperties = nil then
    Exit;
  Assert(DAField <> nil);

  BuildPropertiesProperty(EditProperties, DAField.CustomAttributes, DAField);

  if Assigned(FOnWrapPropperties) then
    FOnWrapPropperties(AColumn, EditProperties, DAField, DataSource);
end;

procedure TDevExpressModule.BuildPropertiesProperty(EditProperties
  : TcxCustomEditProperties; CustomAttributes: TStrings; DAField: TDAField);
var
  List: TStringList;
  J: Integer;
  Attr: string;
  FileImages: TFileImageList;

  procedure BuildImageComboBoxItem(AItems: TcxImageComboBoxItems;
    const AText: string; AImages: TFileImageList);
  var
    TextList: TStrings;
  begin
    TextList := TStringList.Create;
    try
      TextList.CommaText := AText;
      with AItems.Add do
      begin
        if TextList.Count > 0 then
          ImageIndex := AImages.IndexOf(TextList[0]);
        if TextList.Count > 1 then
          Description := TextList[1];
        if TextList.Count > 2 then
          Value := TextList[2];
      end;
    finally
      TextList.Free;
    end;
  end;

  procedure BuildEditButtonItem(AButtons: TcxEditButtons; AText: string;
    AImages: TFileImageList);
  var
    TextList: TStrings;
  begin
    TextList := TStringList.Create;
    try
      TextList.CommaText := AText;
      with AButtons.Add do
      begin
        Kind := bkText; // bkGlyph;  // 不能同时显示图标和文本???
        if (TextList.Count > 0) and (AImages <> nil) then
          ImageIndex := AImages.IndexOf(TextList[0]);
        if TextList.Count > 1 then
        begin
          Caption := TextList[1];
          Hint := Caption;
        end;
      end;
    finally
      TextList.Free;
    end;
  end;

begin

  EditProperties.ReadOnly := not StrToBoolDef
    (CustomAttributes.Values['Options.Editing'], True);
  EditProperties.ImmediatePost :=
    StrToBoolDef(CustomAttributes.Values['Properties.ImmediatePost'], False);

  // 设置输入法
  if EditProperties.InheritsFrom(TcxCustomTextEditProperties) then
  begin
    with TcxCustomTextEditProperties(EditProperties) do
    begin
      Attr := CustomAttributes.Values['Properties.ImeMode'];
      if Attr <> '' then
      begin
        ImeMode := TImeMode(GetEnumValue(TypeInfo(TImeMode), Attr));

        if ImeMode = imOpen then
          ImeName := FDefaultIME;
      end;
    end
  end;

  if EditProperties.InheritsFrom(TcxTextEditProperties) then
    with TcxTextEditProperties(EditProperties) do
    begin
      Attr := CustomAttributes.Values['Properties.EchoMode'];
      if Attr <> '' then
        EchoMode := TcxEditEchoMode
          (GetEnumValue(TypeInfo(TcxEditEchoMode), Attr));
      Attr := CustomAttributes.Values['Properties.PasswordChar'];
      if Length(Attr) > 0 then
        PasswordChar := Attr[1];
      // Attr := DAField.CustomAttributes.Values['Properties.CharCase'];
      LookupItems.Delimiter := ';';
      LookupItems.DelimitedText := CustomAttributes.Values
        ['Properties.LookupItems'];
    end
  else if EditProperties.InheritsFrom(TcxCheckBoxProperties) then
    with TcxCheckBoxProperties(EditProperties) do
    begin
      if DAField.DataType in [datInteger, datLargeInt, datByte, datShortInt,
        datWord, datSmallInt, datCardinal, datLargeUInt] then
      begin
        // 设置数值型字段的布尔值
        ValueChecked := 1;
        ValueUnChecked := 0;
      end
    end
  else if EditProperties.InheritsFrom(TcxCurrencyEditProperties) then
    with TcxCurrencyEditProperties(EditProperties) do
    begin
      DisplayFormat := CustomAttributes.Values['Properties.DisplayFormat'];
      if DisplayFormat = '' then
        DisplayFormat := '0.00';
      // DAField.DisplayFormat; Schema中直接设置DisplayFormat后，字段显示不正常
      EditFormat := DisplayFormat;
      DecimalPlaces :=
        StrToIntDef(CustomAttributes.Values['Properties.DecimalPlaces'], 2);
      Attr := CustomAttributes.Values['Properties.MaxValue'];
      if Attr <> '' then
        MaxValue := StrToIntDef(Attr, 0);
      Attr := CustomAttributes.Values['Properties.MinValue'];
      if Attr <> '' then
        MinValue := StrToIntDef(Attr, 0);
    end
  else if EditProperties.InheritsFrom(TcxImageComboBoxProperties) then
    with TcxImageComboBoxProperties(EditProperties), CustomAttributes do
    begin
      Items.Clear;
      ShowDescriptions :=
        StrToBoolDef(Values['Properties.ShowDescriptions'], True);
      {
        Properties.Images.Width=24
        Properties.Images.Height=16
        Properties.Items=ImageName, Description, Value; ImageName, Description, Value;...
      }
      {
        FileImages := TFileImageList.Create(Column, AppCore.ImagePath, '', sDefaultImageName,
        StrToIntDef(Values['Properties.Images.Width'], 16),
        StrToIntDef(Values['Properties.Images.Height'], 16));
      }
      FileImages := AppCore.SmallImage;

      Images := FileImages.ImageList;
      List := TStringList.Create;
      try
        List.Delimiter := ';';
        List.DelimitedText := Values['Properties.ImageItems'];
        while List.Count > 0 do
        begin
          BuildImageComboBoxItem(Items, List[0], FileImages);
          List.Delete(0);
        end;
      finally
        List.Free;
      end;
    end
  else if EditProperties.InheritsFrom(TcxDateEditProperties) then
    with TcxDateEditProperties(EditProperties), CustomAttributes do
    begin
      SaveTime := StrToBoolDef(Values['Properties.SaveTime'], False);
      ShowTime := SaveTime;
      if SaveTime then
      begin
        // InputKind :=
        Kind := ckDateTime;
      end;
    end
  else if EditProperties.InheritsFrom(TcxPopupEditProperties) then
    with TcxPopupEditProperties(EditProperties) do
    begin
      // PopupClientEdge := True;
      // ReadOnly := True;  // 防止清除数据，但显示为灰色
    end
  else if EditProperties.InheritsFrom(TcxComboBoxProperties) then
    with TcxComboBoxProperties(EditProperties) do
    begin
      ImmediateDropDownWhenActivated := True;
      Items.Delimiter := ';';
      Items.DelimitedText := CustomAttributes.Values['Properties.Items'];
      Attr := CustomAttributes.Values['Properties.DropDownListStyle'];
      if Attr <> '' then
        DropDownListStyle := TcxEditDropDownListStyle
          (GetEnumValue(TypeInfo(TcxEditDropDownListStyle), Attr));
    end
  else if EditProperties.InheritsFrom(TcxLookupComboBoxProperties) then
    with TcxLookupComboBoxProperties(EditProperties) do
    begin
      DropDownAutoSize := True;
      DropDownSizeable := True;
      // ImmediateDropDownWhenActivated := True;
      ListFieldNames := CustomAttributes.Values['Properties.ListFieldNames'];
      ListFieldIndex :=
        StrToIntDef(CustomAttributes.Values['Properties.ListFieldIndex'], 0);
      if Assigned(FOnGetLookupSource) then
      begin
        if Assigned(FOnGetLookupSource) then
        begin
          ListSource := FOnGetLookupSource
            (CustomAttributes.Values['Properties.ListSource']);
          if ListSource <> nil then
            KeyFieldNames := TDADataSource(ListSource)
              .DataTable.CustomAttributes.Values['KeyFieldNames'];
        end;
      end;
    end
  else if EditProperties.InheritsFrom(TcxButtonEditProperties) then
    with TcxButtonEditProperties(EditProperties), CustomAttributes do
    begin
      Attr := CustomAttributes.Values['Properties.ViewStyle'];
      if Attr <> '' then
        ViewStyle := TcxTextEditViewStyle
          (GetEnumValue(TypeInfo(TcxTextEditViewStyle), Attr));

      {
        FileImages := TFileImageList.Create(Column, AppCore.ImagePath, '', sDefaultImageName,
        StrToIntDef(Values['Properties.Images.Width'], 16),
        StrToIntDef(Values['Properties.Images.Height'], 16));
      }
      FileImages := AppCore.SmallImage;

      Images := FileImages.ImageList;

      List := TStringList.Create;
      try
        List.Delimiter := ';';
        List.DelimitedText := Values['Properties.Buttons'];
        Buttons.Clear;
        while List.Count > 0 do
        begin
          BuildEditButtonItem(Buttons, List[0], FileImages);
          List.Delete(0);
        end;
      finally
        List.Free;
      end;
    end
  else if EditProperties.InheritsFrom(TcxHyperLinkEditProperties) then
    with TcxHyperLinkEditProperties(EditProperties), CustomAttributes do
    begin
      SingleClick := True;
    end
  else if EditProperties.InheritsFrom(TcxMaskEditProperties) then
    with TcxMaskEditProperties(EditProperties) do
    begin
      MaskKind := emkRegExprEx;
      EditMask := CustomAttributes.Values['Properties.EditMask'];
    end
  else if EditProperties.InheritsFrom(TcxCalcEditProperties) then
    with TcxCalcEditProperties(EditProperties) do
    begin
      DisplayFormat := CustomAttributes.Values['Properties.DisplayFormat'];
    end
  else if EditProperties.InheritsFrom(TcxSpinEditProperties) then
    with TcxSpinEditProperties(EditProperties) do
    begin
      // if DAField.DataType in [datFloat, datCurrency, datDecimal, datSingleFloat] then
      ValueType := vtFloat;
      // else
      // ValueType := vtInt;
      DisplayFormat := CustomAttributes.Values['Properties.DisplayFormat'];
      // if DisplayFormat = '' then DisplayFormat := '0.00';
      EditFormat := DisplayFormat;
      Attr := CustomAttributes.Values['Properties.MaxValue'];
      if Attr <> '' then
        MaxValue := StrToIntDef(Attr, 0);
      Attr := CustomAttributes.Values['Properties.MinValue'];
      if Attr <> '' then
        MinValue := StrToIntDef(Attr, 0);
    end
  else if EditProperties.InheritsFrom(TcxMemoProperties) then
    with TcxMemoProperties(EditProperties) do
    begin
      VisibleLineCount := 0;
      Attr := CustomAttributes.Values['Properties.VisibleLineCount'];
      if Attr <> '' then
        VisibleLineCount := StrToIntDef(Attr, 2);
      Attr := CustomAttributes.Values['Properties.ScrollBars'];
      if Attr <> '' then
        ScrollBars := TcxScrollStyle
          (GetEnumValue(TypeInfo(TcxScrollStyle), Attr))
      else
        ScrollBars := ssVertical;
    end
  else if EditProperties.InheritsFrom(TcxCheckComboBoxProperties) then
    with TcxCheckComboBoxProperties(EditProperties) do
    begin
      Items.Clear;
      List := TStringList.Create;
      try
        List.Delimiter := ';';
        List.DelimitedText := CustomAttributes.Values['Properties.Items'];
        for J := 0 to List.Count - 1 do
          Items.AddCheckItem(List[J]);
      finally
        FreeAndNil(List);
      end;
    end
  else if EditProperties.InheritsFrom(TcxCheckGroupProperties) then
    with TcxCheckGroupProperties(EditProperties) do
    begin
      Items.Clear;
      Columns := StrToIntDef(CustomAttributes.Values['Properties.Columns'], 1);
      EditValueFormat := cvfInteger; // 此种格式最方便
      List := TStringList.Create;
      try
        List.Delimiter := ';';
        List.DelimitedText := CustomAttributes.Values['Properties.Items'];
        for J := 0 to List.Count - 1 do
          Items.Add.Caption := List[J];
      finally
        FreeAndNil(List);
      end;
    end
  else if EditProperties.InheritsFrom(TcxRadioGroupProperties) then
    with TcxRadioGroupProperties(EditProperties) do
    begin
      Items.Clear;
      Columns := StrToIntDef(CustomAttributes.Values['Properties.Columns'], 1);
      List := TStringList.Create;
      try
        List.Delimiter := ';';
        List.DelimitedText := CustomAttributes.Values['Properties.Items'];
        for J := 0 to List.Count - 1 do
          with Items.Add do
          begin
            Caption := List.Text[J];
            Value := List.ValueFromIndex[J];
          end;
      finally
        FreeAndNil(List);
      end;
    end
  else if EditProperties.InheritsFrom(TcxImageProperties) then
    with TcxImageProperties(EditProperties) do
    begin
      GraphicClassName := 'TdxSmartImage';
    end;
end;

```

这里精彩的地方是如何把Schema里面的CustomAttributes一步步传递到DevExpress控件的TcxCustomEditProperties。

〉请大家注意，上面的代码对TcxPopupEditProperties我们没有设置任何属性。这个弹出式编辑框要展示的内容(其他控件或窗体)，由TPopupEditorWrapper通过DevExpress.OnWrapProperties事件接管。TPopupEditorWrapper是个包装类，这种模式不修改源码，并且可组合多个第3方组件，扩展功能的同时，提高了代码复用，可维护性也有保障。整个框架有几处应用这种模式！

```delphi
constructor TPopupEditorWrapper.Create(AOwner: TComponent);
begin
  inherited;
  // 其他Wrapper都要记下这个属性，以便传递事件
  FOldOnWrapProperties := DevExpress.OnWrapProperties;
  DevExpress.OnWrapProperties := WrapProperites;
  FRegisteredEditor := TStringList.Create;
  FCreatedEditor := TStringList.Create;
  RegisterPopupEditor(sPopupViewName_DictIME, TDictIMEDialog);     // 字典输入方式
  RegisterPopupEditor(sPopupViewName_AgeIME, TAgeIMEDialog);
end;
```


下面查看HisService_Clinic.daSchema里面的Clin_Reg_Help，该DataTable用于创建挂号视图。DataTable CustomAttributes：
```yaml
DefaultBeginsLayer=False                  #布局是否开始新层
FormControlWidth=200                      #控件默认宽度
FormImageName=misc\clin_reg.png           
```

字段 OfficeName CustomAttributes：
```yaml
BeginsLayer=True
Properties.IMEMode=imClose
Style.Font.Size=20
Properties=PopupEdit                      #使用弹出式编辑框                
IME.DictName=Sys_ClinicOffice             #弹出框展示门诊科室字典
IME.GetValueField=OfficeID;OfficeName     #赋值时取字典的两个字段OfficeID和OfficeName
IME.SetValueField=OfficeID;OfficeName     #赋值到目标DataTable的两个字段OfficeID和OfficeName 
```

字段DiagPrice CustomAttributes：
```yaml
Options.Editing=False                     #是否可编辑
Style.Font.Size=20
Properties=CurrencyEdit                   #使用CurrencyEdit控件
Properties.DecimalPlaces=2                #保留两位小数
Properties.DisplayFormat=0.00             #显示格式
```

最后我们可以检查 ***doc/Schema自定义属性说明.xls*** 文件，看看哪些地方可以设置哪些属性。

