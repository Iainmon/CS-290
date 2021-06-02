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
        },
        Entry {
            title="Assignment 3",
            date="May 10, 2021",
            questions=[
                ("What was challenging about the assignment, and what specific kinds of problems did you have.  How did you solve those problems?"
                , unlines [
                    "The most challenging part of this assignment was deciding what level of abstraction I should use to design my rendering system. "
                    ,"Since searching required the nodes be removed (not just hidden) from the DOM, I needed a way to store them so that they could be added back in when a user searched for something else. "
                    ,"I considered saving each twit element and removing it from the DOM using <code>.remove()</code>, but keeping it in memory. Then whenever a new twit is created, I would clone a twit and add it to the list, as well as the DOM."
                    ,"I decided against this because searching would require quite a bit of accesses for each twit element in memory, and didn't resemble the architecture of most front-end frameworks (e.g. React). "
                    ,"Instead, once the page loads, I save the twit & author pairs to a list of <code>{ content: '...', author: '...' }</code> objects, clone a copy of the first twit in order to use as a template for new twits, and clear and re-render the page when any search happens."
                    ,"This isn't the fastest solution since new nodes are being created on every key press, but could very well be optimized just by using a list of cached DOM objects that would mirror the list of twit & author pairs."
                ]),
                ("What did you learn from the assignment?  Were there any special insights you had?  What did you find that you already knew?"
                , unlines [
                    "I learned that <code>.querySelector('.className')</code> returns the first element with that class name. "
                    ,"One insight was to apply functional programming concepts to my code in order to make it more readable. For example, <code> const show = id => document.getElementById(id).classList.remove('hidden') </code> made my code much more readable."
                    ,"It was helpful that I already had a good understanding of event listeners since they allowed me to not have to write so much code and specialize the site behaviors."
                ]),
                ("What kinds of resources were helpful for completing the assignment?  Specific websites?  Lectures?  The class Piazza forum?  The TAs?  How did you use each of these resources?"
                , unlines [
                    "Lucas was very speedy when answering a small grading question, and CampusWire served me well when I needed clarification about a requirement."
                ]),
                ("What are one or two things you had to Google to complete the assignment?"
                , unlines [
                    "I had to consult Mr. Google about Javascript's <code>string.trim()</code> functionality and <code>node.remove()</code> since I lacked a clear understanding of the functions."
                ])
            ]
        },
        Entry {
            title="Assignment 4",
            date="May 17, 2021",
            questions=[
                ("What did you learn from the assignment?  Were there any special insights you had?  What did you find that you already knew?"
                , unlines [
                    "I learned quite a bit about error handling with Promises. I have worked with JavaScript's Promise functionality quite a bit in the past, but I realized I have been making a some fundamental assumptions. "
                    ,"I assumed that Promises followed the axioms of Monads, and behaved very similar to the Maybe monad. This was incorrect. I learned that when handling errors with the <code>.catch( e => ...)</code> method, the Promise object that it returns is not handled by any other <code>.catch( e => ...)</code>."
                    ,"This was an issue for me since I wanted to compose two different behaviors for my server: if there was an error, if everything went smoothly."
                    ,"Once I noticed this, I fixed my code by adding the identity transformation for errors: <code>x => {throw x;}</code>. When used in a .catch handler, the program logic will remain inside the <code>.catch(...)</code> handlers."
                    ,"After getting my original program to work, I wanted to try to make my program into one large expression, using only promises, and no <code>const, var, let</code> variable assignments. "
                    ,"I didn't think it could be done since there needs to be a mutable state somewhere in the code to store the loaded in files, but then I remembered JavaScript uses pass-by-reference for non-primitives. "
                    ,"Using this, I just declared a <code>new Map()</code> and passed it into a lambda that would bind it to the scope so I could mutate it."
                ]),
                ("What are one or two things you had to Google to complete the assignment?"
                , unlines [
                    "I googled the Mozilla documentation for Promises so I could learn more about them."
                    ,"I also looked up the standard MIME types because I tend to forget them. "
                ])
            ]
        },
        Entry {
            title="Assignment 5",
            date="June 1, 2021",
            questions=[
                ("What was challenging about the assignment, and what specific kinds of problems did you have. How did you solve those problems?"
                , unlines [
                    "This assignment was quite challenging due the way Handlebars searches for views and templates. It took me a while to figure out how to properly configure my file system and get everything working."
                    ,"I also found that there is very little information provided on the handlebars-express package and it was difficult to figure out how to tie everything together."
                    ,"Watching the lecture videos was helped me figure out my issue."
                ]),
                ("What did you learn from the assignment? Were there any special insights you had? What did you find that you already knew?"
                , unlines [
                    "I had not used handlebars before this assignment. Already having an understanding of Express.js was very helpful for solving the problems I had."
                    ,"It was also very important for me to learn the hierarchy of layouts, views, and partials since the terminology changes for every web framework."
                ])
            ]
        }
    ]