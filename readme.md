# shaqal

*shaqal* is an SQL database abstractions toolkit for Scala, implemented on top of JDBC.

## Tutorial

### Simple data access

Suppose we have a database with a table `Person`, with three columns: `id` (int), `name` (varchar), and `age` (nullable int).

To use *shaqal* to access this table, the first thing we would do is to set up an abstraction for it, here by using the basic `Accessor` trait:

```scala
trait PersonAccessor extends Accessor {
  
  val id = new int("id") with notnull
  val name = new varchar("name") with notnull
  val age = new int("age") with nullable
  
  def fields = Seq(id, name, age)
}
```

Then we tie this table abstraction to a specific table on a database abstraction:

```scala
class BasicDB extends Database with DefaultSchema {

  object Person extends Table("Person") with PersonAccessor
}
```

And finally create an instance of this database abstraction:

```scala
object BasicDB extends BasicDB { type D = BasicDB }
```

To connect to an actual database, we need a `DBC` (database connector) object. There are several ways to get such an object, based on what mechanism will be used to connect to the database and how the server name and credentials and such will be specified. For this example, we will be using an H2 in-memory database engine and connect to it with a standard JDBC URL:

```scala
object BasicDBC extends UrlDBC[BasicDB](
  "jdbc:h2:mem:basicdb",
  new org.h2.Driver,
  "user",
  "password") {
  
  def adapter = H2Adapter
}
```

Finally, for all operations on database abstractions, an implicit reference to the connector must be available:

```scala
implicit val dbc = BasicDBC
```

(As can be seen above, this object is parameterized with the type of the database abstraction, and as such it is possible to work with several different databases in the same scope.)

Now we are ready to start accessing the data in the database.

First we can insert some data:

```scala
  BasicDB.Person insert BasicDB.Person.Values(p => Seq(
    p.id := 1,
    p.name := "John",
    p.age := Some(30)))
```

Read data:

```scala
val name = BasicDB.Person where(_.id is 1) select(_.name) option()
```

Update:

```scala
BasicDB.Person updateWhere(_.id is 1) set BasicDB.Person.Value(_.age := Some(40))
```

And delete:

```scala
BasicDB.Person deleteWhere(_.id is 1)
```

And use aggregate functions:

```scala
val numPersons = BasicDB.Person count()
```

### Mapping to data models

Usually we want to use classes to hold the data in our database tables, and set up automatic mapping between a table and a corresponding class.

For the example above, we could use a model class like this:

```scala
case class Person(id: Int, name: String, age: Option[Int])
```

(It doesn't have to be a case class.)

We can then use the `Mapper` trait in place of the `Accessor` trait and add mapping logic inside it:

```scala
trait PersonMapper extends Mapper[Person] {

  val id = new int("id") with notnull
  val name = new varchar("name") with notnull
  val age = new int("age") with nullable

  def fields = Seq(id, name, age)

  val (reader, writer) = RW(
    implicit rs => Person(id.read, name.read, age.read),
    p => Seq(id := p.id, name := p.name, age := p.age))
}
```

(Note that we use the same name for both the `Person` data class and the `Person` table object, however we have made sure that they exists in different namespaces.)

Now we can insert data like this:

```scala
  BasicDB.Person insert Person(2, "Bob", Some(34))
  
  val ann = Person(3, "Ann", Some(57))
  val lisa = Person(4, "Lisa", None)
  
  BasicDB.Person insertAll Seq(ann, lisa)
```

And read data:

```scala
  val person = BasicDB.Person where(_.id is 3) option()
  
  val allPersons = BasicDB.Person list()
```

We can of course also use all the operations shown previously.

### Creating tables

There are two basic scenarios when working with databases: the database already exists, or we create the database ourselves.

In the second scenario it can be useful to have the database abstraction tool take care of creating the tables in the database. We can do this with *shaqal* by mixing in the `TableDefinition` trait to the table abstraction:

```scala
trait PersonAccessor extends Accessor with TableDefinition {

  val id = new int("id") with notnull
  val name = new varchar(500)("name") with notnull
  val age = new int("age") with nullable

  def fields = Seq(id, name, age)
  
  def constraints = Nil  
}
```

We have to add a `constraints` method which we for now just set to `Nil`. We'll see how to add constraints to the table later. (Also note that we added an optional length parameter to the varchar type for `name`.)

Now we can create the table:

```scala
  BasicDB.Person create()
```

### Using primary keys




