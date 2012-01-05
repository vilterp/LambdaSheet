package lambdasheet

import javafx.stage.Stage
import javafx.scene.layout.VBox
import javafx.scene.Scene
import javafx.scene.paint.Color
import javafx.collections.FXCollections
import javafx.event.{ActionEvent, EventHandler}
import javafx.geometry.Insets
import javafx.scene.input.{KeyEvent, KeyCode}
import javafx.util.Callback
import javafx.scene.control._
import cell.PropertyValueFactory
import javafx.beans.property.{SimpleObjectProperty, SimpleStringProperty}

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

    val nameCol = new TableColumn[Binding, String]("Name")
    nameCol.setCellValueFactory(new PropertyValueFactory[Binding, String]("name"))
    nameCol.setCellFactory(new Callback[TableColumn[Binding, String], TableCell[Binding, String]] {
      def call(column:TableColumn[Binding, String]):TableCell[Binding, String] = {
        new StringEditCell[Binding]
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
    exprCol.setMinWidth(200)
    tv.getColumns.add(exprCol)

    tv.setEditable(true)

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

  def this() = this(null)

  def parse(value: String): T

  def serialize(value: T): String

  override def startEdit() {
    super.startEdit()
    textField = new TextField
    textField.setMinWidth(this.getWidth - this.getGraphicTextGap * 2)
    textField.setText(serialize(this.getItem))
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
      }
    })
    this.setGraphic(textField)
    this.setContentDisplay(ContentDisplay.GRAPHIC_ONLY)
  }

  override def cancelEdit() {
    super.cancelEdit()
    setContentDisplay(ContentDisplay.TEXT_ONLY)
    textField = null
    println("cancel edit")
  }

  override def commitEdit(item: T) {
    super.commitEdit(item)
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

}
