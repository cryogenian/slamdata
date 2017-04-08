"use strict"

const cache = require("gulp-cached")
const changeCase = require("change-case")
const cheerio = require("gulp-cheerio")
const fs = require("fs")
const glob = require("glob")
const gulp = require("gulp")
const inject = require("gulp-inject")
const path = require("path")
const svgSprite = require("gulp-svg-sprite")


function injectIconsIntoHTML() {
  // Srsly TODO: figure out how to use this to legally attribute authors
  const iconAttribution = {}

  const pathRemoveAttrs = [
    "color",
    "font-family",
    "font-weight",
    "overflow",
    "style"
  ]

  const svgSymbols =
    gulp.src("icons/**/*.svg")
      .pipe(cache("iconing"))
      // adjust SVGs from Project Noun
      .pipe(cheerio({
        parserOptions: { xmlMode: true },
        run: ($, file) => {
          const name = changeCase.titleCase(path.parse(file.path).name)
          const svg = $("svg")
          // inject the title for accessibility
          svg.prepend(`<title>${name}</title>`)

          const text = svg.find("text")

          // not the best check, but I'm assuming these are Noun Project
          // files
          if (text.length > 0) {
            const vs = svg.attr("viewBox").match(/\d+/g)

            if (vs.length === 4) {
              // squaring up the viewbox based on smaller value
              const size = Math.min(vs[2], vs[3])
              svg.attr("viewBox", `0 0 ${size} ${size}`)
            }

            // push icon into the attribution object
            const a = text.map(function() {
              return $(this).text()
            }).get().join(" ")

            iconAttribution[a] = a in iconAttribution
              ? iconAttribution[a].concat(name)
              : iconAttribution[a] = [name]
          }

          text.remove()

          // these irrelevant attrs need to be removed
          const path_ = svg.find("path")
          pathRemoveAttrs.forEach((attr) => path_.removeAttr(attr))
        }
      }))
      // Transform the SVGs into a symbol sprite
      .pipe(svgSprite({
        svg: {
          xmlDeclaration: false,
          doctypeDeclaration: false,
          rootAttributes: {
            "aria-hidden": "true",
            style: "display:none"
          }
        },
        shape: {
          dimensions: { maxWidth: 100, maxHeight: 100, precision: 3 },
          id: {
            // `file.stem` not supported in this vinyl version :/
            // using this to override the default behavior of adding
            // the directory to the id
            generator: (name, file) =>
              `sd-icon--${path.parse(name).name.replace(/\s/g, "-")}`
          }
        },
        mode: {
          symbol: {
            render: { css: false, scss: false },
          }
        }
      }))

  // inject symbols into html files
  gulp.src("html/**/*.html")
    .pipe(inject(svgSymbols, {
      starttag: "<!-- icon-symbols -->",
      endtag: "<!-- /icon-symbols -->",
      removeTags: true,
      transform: (filePath, file) => file.contents.toString("utf8")
    }))
    .pipe(gulp.dest('./public'))
}


function createIconPureScript() {
  glob("icons/**/*.svg", {}, (err, files) => {
    if (err) {
      throw err
      return
    }

    const { exports, html } = files.reduce((acc, file) => {
      // get unique file names
      const p = path.parse(file)

      if (p == null) {
        console.error("[icon-purs-adt]", file, "didn't parse.")
      }

      const name = p.name.replace(/\s/g, "-")

      if (!acc.includes(name)) {
        acc.push(name)
      } else {
        console.warn(
          "[icon-purs-adt]:",
          name,
          "from",
          file,
          "is a duplicate name & is being ignored."
        )
      }

      return acc
    }, []).reduce((acc, name) => {
      // make an array for exports and one for the HTML
      const camelName = changeCase.camelCase(name)
      const html = `${camelName} ∷ ∀ p i. H.HTML p i
${camelName} = iconHelper "${name}"`
      acc.exports.push(camelName)
      acc.html.push(html)
      return acc
    }, { exports: [], html: [] })

    const pursData = `module SlamData.Render.Icon
  ( ${exports.join("\n  , ")}
  ) where

import SlamData.Prelude

import Halogen.HTML as H
import Halogen.HTML.Properties as P

iconHelper ∷ ∀ p i. String → H.HTML p i
iconHelper s =
  let
    svgElem = H.elementNS $ H.Namespace "http://www.w3.org/2000/svg"
    xlinkAttr = H.attrNS $ H.Namespace "http://www.w3.org/1999/xlink"
  in
    -- Oddly, I suppose do to namespacing, the CSS class on <svg> not
    -- picked up wrapping it seems to work though
    H.i
      [ P.class_ $ H.ClassName $ "sd-icon sd-icon--" <> s ]
      [ svgElem (H.ElemName "svg")
        []
        [ svgElem (H.ElemName "use")
          [ xlinkAttr (H.AttrName "xlink:href") $ "#sd-icon--" <> s ]
          []
        ]
      ]

${html.join("\n\n")}
`

    const path_ = path.join("src", "SlamData", "Render", "Icon.purs")
    fs.writeFileSync(path_, pursData)
  })
}

module.exports = {
  injectIconsIntoHTML,
  createIconPureScript
}
