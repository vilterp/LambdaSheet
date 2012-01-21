package lambdasheet

import javafx.stage.Stage
import javafx.scene.layout.VBox
import javafx.scene.Scene
import javafx.scene.paint.Color
import javafx.collections.FXCollections
import javafx.geometry.Insets
import javafx.util.Callback
import javafx.beans.property.{SimpleObjectProperty, SimpleStringProperty}
import javafx.scene.control.TableColumn.CellEditEvent
import javafx.scene.control._
import cell.PropertyValueFactory
import javafx.event.{EventType, ActionEvent, EventHandler}
import javafx.scene.input.{MouseEvent, KeyEvent, KeyCode}

object LambdaSheet {

  def main(args:Array[String]) {
    javafx.application.Application.launch(classOf[LambdaSheet], args:_*)
  }

}

class LambdaSheet extends javafx.application.Application {

  var i = 0

  def start(stage:Stage) {

    val box = new VBox()
    box.setSpacing(5)

    box.getChildren.add(new Label("Bindings"))
    box.setPadding(new Insets(10))

    // table

    val bindings = FXCollections.observableArrayList[Binding]()

    val tv = new TableView[Binding](bindings)

    tv.setOnKeyReleased(new EventHandler[KeyEvent] {
      def handle(p1: KeyEvent) {
        if(p1.getCode.eq(KeyCode.BACK_SPACE) || p1.getCode.eq(KeyCode.DELETE)) {
          val ind = tv.getSelectionModel.getSelectedIndex
          tv.getItems.remove(ind)
        }
      }
    })
    
    val nameCol = new TableColumn[Binding, String]("Name")
    nameCol.setCellValueFactory(new PropertyValueFactory[Binding, String]("name"))
    nameCol.setCellFactory(new Callback[TableColumn[Binding, String], TableCell[Binding, String]] {
      def call(column:TableColumn[Binding, String]):TableCell[Binding, String] = {
        new StringEditCell[Binding]
      }
    })
    nameCol.setOnEditCommit(new EventHandler[CellEditEvent[Binding, String]] {
      def handle(evt: CellEditEvent[Binding, String]) {
        evt.getRowValue.setName(evt.getNewValue)
        println("name col commit: " + evt.getNewValue)
      }
      override def toString = "name col event handler"
    })
    nameCol.setOnEditStart(new EventHandler[CellEditEvent[Binding, String]] {
      def handle(p1: CellEditEvent[Binding, String]) {
        println("name col edit start")
      }
    })
    nameCol.setMinWidth(200)
    tv.getColumns.add(nameCol)

    val exprCol = new TableColumn[Binding, Expression]("Expression")
    exprCol.setCellValueFactory(new PropertyValueFactory[Binding, Expression]("expr"))
    exprCol.setCellFactory(new Callback[TableColumn[Binding, Expression], TableCell[Binding, Expression]] {
      def call(column:TableColumn[Binding, Expression]):TableCell[Binding, Expression] = {
        new ExprEditCell[Binding]
      }
    })
    exprCol.setOnEditCommit(new EventHandler[CellEditEvent[Binding, Expression]] {
      def handle(evt: CellEditEvent[Binding, Expression]) {
        evt.getRowValue.setExpr(evt.getNewValue)
        println("expr col commit: " + evt.getNewValue)
      }
      override def toString = "expr col commit handler"
    })
    exprCol.setMinWidth(200)
    tv.getColumns.add(exprCol)

    tv.setEditable(true)
    
    println(nameCol.getOnEditCommit)
    println(exprCol.getOnEditCommit)
    println(exprCol.getOnEditStart)

    box.getChildren.add(tv)

    bindings.add(Binding("foo" + i,  IntLit(7)))
    i += 1

    // button

    val addButton = new Button("Add Binding")
    addButton.setOnAction(new EventHandler[ActionEvent](){
      def handle(event:ActionEvent) {
        bindings.add(Binding("foo" + i,  IntLit(i)))
        i += 1
        // TODO: focus name
        // TODO: blank name and expr to start
      }
    })

    box.getChildren.add(addButton)

    // scene, etc

    val scene = new Scene(box, 800, 600, Color.WHITE)

    stage.setScene(scene)

    stage.show()

  }

}

abstract class EditableAsStringCell[S, T](var textField: TextField) extends TableCell[S, T] {

  setOnMouseClicked(new EventHandler[MouseEvent] {
    def handle(p1: MouseEvent) {
      startEdit()
    }
  })
  
  def this() = this(null)

  def parse(value: String): T

  def serialize(value: T): String

  override def startEdit() {
    super.startEdit()
    textField = new TextField
    textField.setMinWidth(this.getWidth - this.getGraphicTextGap * 2)
    if(!isEmpty) {
      textField.setText(serialize(getItem))
    }
    textField.setOnAction(new EventHandler[ActionEvent] {
      def handle(actionEvent: ActionEvent) {
        commitEdit(parse(textField.getText))
      }
    })
    textField.setOnKeyReleased(new EventHandler[KeyEvent] {
      def handle(keyEvent: KeyEvent) {
        if (keyEvent.getCode == KeyCode.ESCAPE) {
          cancelEdit()
        }
        keyEvent.consume()
        println(isFocused)
      }
    })
    this.setGraphic(textField)
    textField.requestFocus()
    println(textField.getCaretPosition)
    this.setContentDisplay(ContentDisplay.GRAPHIC_ONLY)
  }

  override def cancelEdit() {
    super.cancelEdit()
    setContentDisplay(ContentDisplay.TEXT_ONLY)
    textField = null
    println("cancel edit")
  }

  override def commitEdit(item: T) {
    // super.commitEdit(item)
    updateItem(item, false)
    cancelEdit()
    // hack necessitated by seeming bug in JavaFX...
    getTableColumn.getOnEditCommit.handle(new CellEditEvent[S, T](getTableView, new TablePosition(getTableView, this.getTableRow.getIndex, getTableColumn), new EventType[CellEditEvent[_, _]]("EDIT_COMMIT"), item))
    println("commit edit " + item)
  }

  override def updateItem(item: T, empty: Boolean) {
    super.updateItem(item, empty)
    println("update item: " + item + " (" + this + ")")
    if (!empty) {
      setText(serialize(item))
    }
    setContentDisplay(ContentDisplay.TEXT_ONLY)
  }

}

class StringEditCell[S] extends EditableAsStringCell[S, String] {

  def parse(value: String) = value

  def serialize(value: String) = value

}

class ExprEditCell[T] extends EditableAsStringCell[T, Expression] {

  def parse(value: String):Expression =
    ExprParser(value) match {
      case Left((err, loc)) => throw new Exception(err)
      case Right(expr) => expr
    }

  def serialize(value: Expression) = value.toCode

}

object Binding {
  def apply(name:String, expr:Expression):Binding = {
    val nP = new SimpleStringProperty(name)
    val eP = new SimpleObjectProperty[Expression](expr)
    new Binding(nP, eP)
  }
}

case class Binding(nameProperty:SimpleStringProperty, exprProperty:SimpleObjectProperty[Expression]) {

  def setName(newName:String) {
    nameProperty.set(newName)
  }

  def setExpr(newExpr:Expression) {
    exprProperty.set(newExpr)
  }

  var value:LambdaSheetVal = null

}
