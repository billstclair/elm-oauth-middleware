module Server.Html exposing (Document, document, toString)


type Document
    = Document String


document : String -> String -> Document
document title script =
    Document ("""
        <!doctype html>
            <html lang="en">
                <head>
                    <meta charset="utf-8">
                    <title>""" ++ title ++ """</title>
                </head>
                <body>
                    <script>""" ++ script ++ """</script>
                </body>
            </html>
    """)


toString : Document -> String
toString (Document asString) =
    asString
