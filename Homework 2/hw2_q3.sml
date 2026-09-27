Control.Print.printDepth := 100;
Control.Print.printLength := 100;
Control.Print.stringDepth := 1000;

datatype genre =
    Fiction
  | NonFiction
  | Science
  | History
  | Biography
  | Fantasy
  | Mystery
  | Romance
  | Thriller
  | Horror
  | Western
  | Other;

datatype book =
    Book of string * string * int * genre;

datatype reader =
    Reader of string * int
  | Globglogabgalab
  | Anonymous of int;

datatype bookReading =
    BookReading of book * reader * int;

fun genreToString Fiction = "Fiction"
  | genreToString NonFiction = "NonFiction"
  | genreToString Science = "Science"
  | genreToString History = "History"
  | genreToString Biography = "Biography"
  | genreToString Fantasy = "Fantasy"
  | genreToString Mystery = "Mystery"
  | genreToString Romance = "Romance"
  | genreToString Thriller = "Thriller"
  | genreToString Horror = "Horror"
  | genreToString Western = "Western"
  | genreToString Other = "Other";

fun makeDescription readerName bookName author year genre rating suffix =
    readerName
    ^ " read \""
    ^ bookName
    ^ "\" by "
    ^ author
    ^ " in "
    ^ Int.toString year
    ^ " - Genre: "
    ^ genreToString genre
    ^ " - Rating: "
    ^ Int.toString rating
    ^ "/10"
    ^ suffix;

fun describeBookReading
    (BookReading (Book (bookName, author, year, genre), Globglogabgalab, rating)) =
    makeDescription
        "Globglogabgalab"
        bookName
        author
        year
        genre
        10
        (case genre of
             Thriller => " splendid!"
           | _ => "")

  | describeBookReading
    (BookReading (Book (bookName, author, year, Other), Anonymous age, rating)) =
    "Someone read something"

  | describeBookReading
    (BookReading (Book (bookName, author, year, Western), Anonymous age, rating)) =
    makeDescription
        "cowboy"
        bookName
        author
        year
        Western
        rating
        ""

  | describeBookReading
    (BookReading (Book (bookName, author, year, Horror), Anonymous age, rating)) =
    makeDescription
        (Int.toString age ^ "-year-old reader")
        bookName
        author
        year
        Horror
        (if age < 18 then 5 else 8)
        ""

  | describeBookReading
    (BookReading (Book (bookName, author, year, genre), Anonymous age, rating)) =
    makeDescription
        (Int.toString age ^ "-year-old reader")
        bookName
        author
        year
        genre
        rating
        ""

  | describeBookReading
    (BookReading (Book (bookName, author, year, Horror), Reader (name, age), rating)) =
    makeDescription
        name
        bookName
        author
        year
        Horror
        (if age < 18 then 5 else rating)
        ""

  | describeBookReading
    (BookReading (Book (bookName, author, year, genre), Reader (name, age), rating)) =
    makeDescription
        name
        bookName
        author
        year
        genre
        rating
        "";