import Data.List.Split
import Control.Monad (when)

type Question = (String,String) -- Question, Answer

data BlogEntry = Entry {
        title :: String,
        date :: String,
        questions :: [Question]
    } deriving (Eq)


bold s = "<b>" ++ s ++ "</b>"
para s = "<p>" ++ s ++ "</p>"

uList lItems = "<ul>" ++ (unlines $ map (\li -> "<li>" ++ li ++ "</li>") lItems) ++ "</ul>"

styleQuestionAnswer :: Question -> String
styleQuestionAnswer q = uList [
        bold (fst q),
        para (snd q)
    ]

styledHeader n str = "<h" ++ show n ++ " class=\"display-" ++ show n ++"\">" ++ str ++ "</h" ++ show n ++ ">"

tag name [] = \x -> "<" ++ name ++ ">" ++ x ++ "</" ++ name ++ ">"
tag name classes = \x -> "<" ++ name ++ " class=\"" ++ (foldl (++) "" $ map (++" ") classes) ++ "\"" ++">" ++ x ++ "</" ++ name ++ ">"




instance Show BlogEntry where
    show entry = unlines [
            "<div class=\"row\">"
            , tag "div" ["d-flex", "justify-content-between"] $ styledHeader 5 (title entry)  ++ styledHeader 6 (date entry)
            ,unlines $ map styleQuestionAnswer $ questions entry
            ,"</div>"
        ]

compileBlog = unlines . map ("<hr>"++) . map show


boilerplate :: [String] -> String
boilerplate sources@[presource,source,postsource] = unlines sources


main = do
    rawBoilerPlate <- readFile "boilerplate.html"
    let compiledBlogs = compileBlog blog
    let [presource,postsource] = splitOn "<!--SPLIT-->" rawBoilerPlate
    let compiledSource = boilerplate [presource,compiledBlogs,postsource]
    save compiledSource "blog.html"

save :: String -> String -> IO ()
save source fileName = do
    when (length source > 0) $
        writeFile fileName source








blog :: [BlogEntry]

blog = reverse [
        Entry {
            title="Assignment 1",
            date="April 12, 2021",
            questions=[
                ("What challenges or troubles did you have completing this assignment. How did you work through them?"
                , unlines [
                    "I wanted to get some bouncy text working, but the easiest solution that I found required me to write <<code>span</code>> tags for each character I wanted to animate."
                    ,"I also needed to compute and write a keyframe for each n/1s, n times, where n is the number of characters in the string that I wanted to animate."
                    ,"I got around this by writing a Haskell script that would parse my input string and give me the HTML and CSS for my desired animation."
                ]),
                ("What did you learn from this assignment? What did you already know?"
                , unlines [
                    "I have almost four years amount of full-stack experience, but I did learn more about the Bootstrap 5 framework."
                    ,"I wasn't aware that Bootstrap had classes for images, that turned out to be usefull."
                ]),
                ("What resources (e.g. specific web articles, the class Piazza forum, the TAs) were most helpful in completing this assignment? How did you use these resources?"
                , unlines [
                    "I asked the Campuswire forum about if it were possible to use other technologies such as Typescript or React."
                    ,"Some students were helpful and pointed me to the sylabus, and Professor Hess was able to answer my question."
                ])
            ]
        },
        Entry {
            title="Assignment 2",
            date="April 26, 2021",
            questions=[
                ("What was challenging about the assignment, and what specific kinds of problems did you have.  How did you solve those problems?"
                , unlines [
                    "The most challenging part was styling the premade HTML file using only CSS. Usually I am able to use CSS and throw in HTML div spacers, but I had to do everything by hand."
                    ,"It was certainly challenging, but I feel like I learned a lot about CSS I didn't know!"
                ]),
                ("What are one or two things you had to Google to complete the assignment?"
                , unlines [
                    "I had to research flexboxes quite a bit. Specifically, how to pull an inline flex item all the way to the right, while the other siblings are pulled to the left."
                    ,"I also needed to lean how to make a shadow (for the create twit button)."
                ])
            ]
        }
    ]