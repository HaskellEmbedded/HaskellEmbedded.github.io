{-|
Module      : Main
Description : Static HTML generation for HaskellEmbedded blog
Copyright   : (c) 2015 Calvin Beck, Chris Hodapp, Shae Erisson
License     : ??
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           System.FilePath

-- | Configuration for the RSS and ATOM feeds.
feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle = "Haskell Embedded: Functional Programming and Embedded Systems"
    , feedDescription = "A blog about Haskell / functional programming and embedded systems."
    , feedAuthorName = ""
    , feedAuthorEmail = ""
    , feedRoot = "http://haskellembedded.github.io/"
    }

-- | Main entry point for generating all code via Hakyll
main :: IO ()
main = hakyll $ do
    -- Build up tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")
    let postCtxTags = postCtxWithTags tags

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler
    
    match "images/favicons/*" $ do
        route $ customRoute $ takeFileName . toFilePath
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "pages/*" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "slides/**" $ version "raw" $ do
        route idRoute
        compile copyFileCompiler

    -- We want .lhs files to be directly accessible, so copy them through unmodified.
    match "posts/*.lhs" $ version "raw" $ do
        route idRoute
        compile $ getResourceBody >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"     postCtxTags
            >>= saveSnapshot "feedContent"
            >>= loadAndApplyTemplate "templates/comments.html" postCtxTags
            >>= loadAndApplyTemplate "templates/default.html"  postCtxTags
            >>= relativizeUrls

    -- We want .lhs files to be directly accessible, so copy them through unmodified.
    match "drafts/*.lhs" $ version "raw" $ do
        route idRoute
        compile $ getResourceBody >>= relativizeUrls

    match "drafts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"     postCtxTags
            >>= loadAndApplyTemplate "templates/default.html"  postCtxTags
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
            let archiveCtx =
                    listField "posts" postCtxTags (return posts) `mappend`
                    constField "title" "Archives"                `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
            let indexCtx =
                    listField "posts" postCtxTags (return posts) `mappend`
                    constField "title" "Home"                    `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler
                                  
    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots ("posts/*" .&&. complement "posts/*.lhs") "feedContent"
            renderAtom feedConfiguration feedCtx posts

    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots ("posts/*" .&&. complement "posts/*.lhs") "feedContent"
            renderRss feedConfiguration feedCtx posts

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title
                      `mappend` listField "posts" postCtx (return posts)
                      `mappend` defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

-- | Post context, no tags present:
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

-- | Post context, with tags:
postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx

-- | Context for feeds.
feedCtx :: Context String
feedCtx = postCtx `mappend` bodyField "description"
