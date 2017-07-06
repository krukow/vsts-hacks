(defproject krukow/vsts-hacks "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.9.0-alpha17"]
                 [org.clojure/clojurescript "1.9.660"]
                 [org.clojure/core.async "0.3.443"]
                 [binaryage/chromex "0.5.8"]
                 [binaryage/devtools "0.9.4"]
                 [cljs-ajax "0.6.0"]
                 [figwheel "0.5.10"]
                 [environ "1.1.0"]
                 [cljsjs/react "15.4.2-2"]
                 [cljsjs/react-dom "15.4.2-2"]
                 [sablono "0.8.0"]
                 [org.omcljs/om "1.0.0-alpha34"]]

  :plugins [[lein-cljsbuild "1.1.6"]
            [lein-figwheel "0.5.10"]
            [lein-shell "0.5.0"]
            [lein-environ "1.1.0"]
            [lein-cooper "1.2.2"]
            [refactor-nrepl "2.3.1"]
            [cider/cider-nrepl "0.14.0"]]

  :source-paths ["src/background"
                 "src/popup"
                 "src/content_script"]

  :clean-targets ^{:protect false} ["target"
                                    "resources/unpacked/compiled"
                                    "resources/release/compiled"]

  :cljsbuild {:builds {}}                                                                                                     ; prevent https://github.com/emezeske/lein-cljsbuild/issues/413

  :profiles {:unpacked
             {:dependencies [[figwheel-sidecar "0.5.8"]
                             [com.cemerick/piggieback "0.2.2"]]

              :cljsbuild {:builds
                          {:background
                           {:source-paths ["src/background"]
                            :figwheel     true
                            :compiler     {:output-to     "resources/unpacked/compiled/background/main.js"
                                           :output-dir    "resources/unpacked/compiled/background"
                                           :asset-path    "compiled/background"
                                           :preloads      [devtools.preload]
                                           :main          vsts-hacks.background
                                           :optimizations :none
                                           :source-map    true}}
                           :popup
                           {:source-paths ["src/popup"]
                            :figwheel     true
                            :compiler     {:output-to     "resources/unpacked/compiled/popup/main.js"
                                           :output-dir    "resources/unpacked/compiled/popup"
                                           :asset-path    "compiled/popup"
                                           :preloads      [devtools.preload]
                                           :main          vsts-hacks.popup
                                           :optimizations :none
                                           :source-map    true}}}}}
             :unpacked-content-script
             {:cljsbuild {:builds
                          {:content-script
                           {:source-paths ["src/content_script"]
                            :compiler     {:output-to     "resources/unpacked/compiled/content-script/main.js"
                                           :output-dir    "resources/unpacked/compiled/content-script"
                                           :asset-path    "compiled/content-script"
                                           :main          vsts-hacks.content-script
                                           ;:optimizations :whitespace                                                        ; content scripts cannot do eval / load script dynamically
                                           :optimizations :advanced                                                           ; let's use advanced build with pseudo-names for now, there seems to be a bug in deps ordering under :whitespace mode
                                           :pseudo-names  true
                                           :pretty-print  true}}}}}
             :checkouts
             ; DON'T FORGET TO UPDATE scripts/ensure-checkouts.sh
             {:cljsbuild {:builds
                          {:background {:source-paths ["checkouts/cljs-devtools/src/lib"
                                                       "checkouts/chromex/src/lib"
                                                       "checkouts/chromex/src/exts"]}
                           :popup      {:source-paths ["checkouts/cljs-devtools/src/lib"
                                                       "checkouts/chromex/src/lib"
                                                       "checkouts/chromex/src/exts"]}}}}
             :checkouts-content-script
             ; DON'T FORGET TO UPDATE scripts/ensure-checkouts.sh
             {:cljsbuild {:builds
                          {:content-script {:source-paths ["checkouts/cljs-devtools/src/lib"
                                                           "checkouts/chromex/src/lib"
                                                           "checkouts/chromex/src/exts"]}}}}

             :figwheel
             {:figwheel {:server-port    6888
                         :server-logfile ".figwheel.log"
                         :nrepl-port 7888
                         :nrepl-middleware ["cider.nrepl/cider-middleware"
                                            "refactor-nrepl.middleware/wrap-refactor"
                                            "cemerick.piggieback/wrap-cljs-repl"]
                         :repl           true}}

             :cooper
             {:cooper {"content-dev" ["lein" "content-dev"]
                       "fig-dev"     ["lein" "fig-dev"]
                       "browser"     ["scripts/launch-test-browser.sh"]}}

             :release
             {:env       {:chromex-elide-verbose-logging "true"}
              :cljsbuild {:builds
                          {:background
                           {:source-paths ["src/background"]
                            :compiler     {:output-to     "resources/release/compiled/background.js"
                                           :output-dir    "resources/release/compiled/background"
                                           :asset-path    "compiled/background"
                                           :main          vsts-hacks.background
                                           ;:optimizations :advanced
                                           :optimizations :simple
                                           :pseudo-names  true
                                           :source-map    "resources/release/compiled/background.js.map"
                                           :elide-asserts true}}
                           :popup
                           {:source-paths ["src/popup"]
                            :compiler     {:output-to     "resources/release/compiled/popup.js"
                                           :output-dir    "resources/release/compiled/popup"
                                           :asset-path    "compiled/popup"
                                           :main          vsts-hacks.popup
                                        ;:optimizations :advanced
                                           :optimizations :simple
                                           :pseudo-names  true
                                           :elide-asserts true}}
                           :content-script
                           {:source-paths ["src/content_script"]
                            :compiler     {:output-to     "resources/release/compiled/content-script.js"
                                           :output-dir    "resources/release/compiled/content-script"
                                           :asset-path    "compiled/content-script"
                                           :main          vsts-hacks.content-script
                                        ;:optimizations :advanced
                                           :optimizations :simple
                                           :source-map    "resources/release/compiled/content-script.js.map"
                                           :pseudo-names  true
                                           :elide-asserts true}}}}}}

  :aliases {"dev-build"   ["with-profile" "+unpacked,+unpacked-content-script,+checkouts,+checkouts-content-script" "cljsbuild" "once"]
            "fig"         ["with-profile" "+unpacked,+figwheel" "figwheel" "background" "popup"]
            "content"     ["with-profile" "+unpacked-content-script" "cljsbuild" "auto" "content-script"]
            "fig-dev"     ["with-profile" "+unpacked,+figwheel,+checkouts" "figwheel" "background" "popup"]
            "content-dev" ["with-profile" "+unpacked-content-script,+checkouts-content-script" "cljsbuild" "auto"]
            "devel"       ["with-profile" "+cooper" "do"                                                                      ; for mac only
                           ["shell" "scripts/ensure-checkouts.sh"]
                           ["cooper"]]
            "release"     ["with-profile" "+release" "do"
                           ["clean"]
                           ["cljsbuild" "once" "background" "popup" "content-script"]]
            "package"     ["shell" "scripts/package.sh"]})
