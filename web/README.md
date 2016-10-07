# web experiments

## References

* [https://platform.html5.org/](https://platform.html5.org/)
* [W3C Markup Validator](https://validator.w3.org/)
* [Performance Validation Tools](https://varvy.com)
* [HTML5 Security Cheat Sheet](https://www.owasp.org/index.php/HTML5_Security_Cheat_Sheet)
* [https://google.github.io/styleguide/htmlcssguide.xml](https://google.github.io/styleguide/htmlcssguide.xml)
* [https://google.github.io/styleguide/javascriptguide.xml](https://google.github.io/styleguide/javascriptguide.xml)

### Web apps without web servers

* [https://www.youtube.com/watch?v=WqV5kqaFRDU](https://www.youtube.com/watch?v=WqV5kqaFRDU)
    * Load while offline
    * Open file from disk
    * Save file to disk
    * New tabs open with current state
    * Old tabs recognize being out of sync
* [https://unhosted.org/adventures/2/An-unhosted-editor.html](https://unhosted.org/adventures/2/An-unhosted-editor.html)
* [http://nobackend.org/](http://nobackend.org/)
* [http://offlinefirst.org/](http://offlinefirst.org/)

#### [Unhosted Apps](https://unhosted.org/apps/)

* [http://tiddlywiki.com/](http://tiddlywiki.com/)
* [https://dspace-nilclass.5apps.com/](https://dspace-nilclass.5apps.com/)
* [http://cloudwall.me/](http://cloudwall.me/)

#### Files

[https://github.com/eligrey/FileSaver.js/](https://github.com/eligrey/FileSaver.js/)

#### AppCache

* [http://www.andygup.net/application-cache-is-not-gone-oh-my-or-is-it/](http://www.andygup.net/application-cache-is-not-gone-oh-my-or-is-it/)
* [http://appcache.offline.technology/](http://appcache.offline.technology/)
* [http://alistapart.com/article/application-cache-is-a-douchebag](http://alistapart.com/article/application-cache-is-a-douchebag)
* [http://labs.ft.com/2012/11/using-an-iframe-to-stop-app-cache-storing-masters/](http://labs.ft.com/2012/11/using-an-iframe-to-stop-app-cache-storing-masters/)

#### Service workers

[Is Service Worker Ready?](https://jakearchibald.github.io/isserviceworkerready/resources.html)
[Introduction to Service Worker](https://developers.google.com/web/fundamentals/primers/service-worker/)

#### IndexedDB

* performance concerns
* [https://www.w3.org/TR/IndexedDB/](https://www.w3.org/TR/IndexedDB/)
* [https://developer.mozilla.org/en-US/docs/Web/API/IndexedDB_API/Using_IndexedDB](https://developer.mozilla.org/en-US/docs/Web/API/IndexedDB_API/Using_IndexedDB)
* [http://blog.vanamco.com/indexeddb-fundamentals-plus-a-indexeddb-example-tutorial/](http://blog.vanamco.com/indexeddb-fundamentals-plus-a-indexeddb-example-tutorial/)

#### Shared state for multiple tabs

* [Sharing sessionStorage](https://blog.guya.net/2015/06/12/sharing-sessionstorage-between-tabs-for-secure-multi-tab-authentication/)
* [Implementation of synchronization primitives over HTML5 local storage](https://stackoverflow.com/a/22283455)

### Communication

#### XHR

[http://www.html5rocks.com/en/tutorials/file/xhr2/](http://www.html5rocks.com/en/tutorials/file/xhr2/)

#### Server sent events

[http://www.html5rocks.com/en/tutorials/eventsource/basics/](http://www.html5rocks.com/en/tutorials/eventsource/basics/)

#### WebSockets

* [http://www.html5rocks.com/en/tutorials/websockets/basics/](http://www.html5rocks.com/en/tutorials/websockets/basics/)
* [https://devcenter.heroku.com/articles/websocket-security](https://devcenter.heroku.com/articles/websocket-security)
    * "If the response is JSON, always use JSON.parse() to safely parse the data."
    * check Origin header

#### WebRTC

* peer-to-peer (browser to browser)
* [Data Channels](http://www.html5rocks.com/en/tutorials/webrtc/datachannels/)
* [https://stackoverflow.com/questions/4118272/do-websockets-allow-for-p2p-browser-to-browser-communication](https://stackoverflow.com/questions/4118272/do-websockets-allow-for-p2p-browser-to-browser-communication)
* [http://peerjs.com/](http://peerjs.com/)
* [https://github.com/js-platform/p2p](https://github.com/js-platform/p2p)
* voice and video
    * [https://web.archive.org/web/20140102203544/https://labs.ericsson.com/developer-community/blog/beyond-html5-peer-peer-conversational-video](https://web.archive.org/web/20140102203544/https://labs.ericsson.com/developer-community/blog/beyond-html5-peer-peer-conversational-video)

#### postMessage

* [https://stackoverflow.com/questions/25271243/window-postmessage-to-communicate-between-applications-in-different-tabs/27234441#27234441](https://stackoverflow.com/questions/25271243/window-postmessage-to-communicate-between-applications-in-different-tabs/27234441#27234441)
* [http://apress.jensimmons.com/v5/pro-html5-programming/ch6.html](http://apress.jensimmons.com/v5/pro-html5-programming/ch6.html)

### Media

[http://www.html5rocks.com/en/tutorials/getusermedia/intro/](http://www.html5rocks.com/en/tutorials/getusermedia/intro/)

#### Visibility

* [PageVisibility API](http://www.html5rocks.com/en/tutorials/pagevisibility/intro/)

#### Webcam

* [https://github.com/jhuckaby/webcamjs/blob/master/webcam.js](https://github.com/jhuckaby/webcamjs/blob/master/webcam.js)
* [https://developer.mozilla.org/en-US/docs/Web/API/WebRTC_API/Taking_still_photos](https://developer.mozilla.org/en-US/docs/Web/API/WebRTC_API/Taking_still_photos)

#### Audio

[http://www.html5rocks.com/en/tutorials/webaudio/intro/](http://www.html5rocks.com/en/tutorials/webaudio/intro/)

#### Graphics

* filters
* [Painting with The HTML5 Canvas](https://gamealchemist.wordpress.com/2014/11/30/203/)

##### WebGL

* [http://learningthreejs.com/blog/2012/02/07/live-video-in-webgl/](http://learningthreejs.com/blog/2012/02/07/live-video-in-webgl/)
* [http://www.html5rocks.com/en/tutorials/three/intro/](http://www.html5rocks.com/en/tutorials/three/intro/)

### Design

#### Inspirations

* [mathematical formulae](http://liamoc.net/posts/2014-01-01-context-split.html)
* [MathBox](https://acko.net/files/gltalks/toolsforthought)
* [http://worrydream.com/substroke/](http://worrydream.com/substroke/)
* [http://worrydream.com/MediaForThinkingTheUnthinkable/](http://worrydream.com/MediaForThinkingTheUnthinkable/)
* [http://worrydream.com/ExplorableExplanations/](http://worrydream.com/ExplorableExplanations/)
* [http://worrydream.com/VisualizingEdgeWeights/](http://worrydream.com/VisualizingEdgeWeights/)
* [http://worrydream.com/DrawingDynamicVisualizationsTalkAddendum/](http://worrydream.com/DrawingDynamicVisualizationsTalkAddendum/)
* [http://worrydream.com/LearnableProgramming/](http://worrydream.com/LearnableProgramming/)
* [http://sediment.io/](http://sediment.io/)
* [http://ckeditor.com/](http://ckeditor.com/)
* [https://www.notion.so/](https://www.notion.so/)
* [http://www.luna-lang.org/](http://www.luna-lang.org/)
* [https://www.spacetime.me/blog/future-programming-workshop-2015](https://www.spacetime.me/blog/future-programming-workshop-2015)
* [http://www.loper-os.org/?p=568](http://www.loper-os.org/?p=568)
* [http://chrisnovello.com/teaching/risd/computer-utopias/](http://chrisnovello.com/teaching/risd/computer-utopias/)
* [https://en.wikipedia.org/wiki/Project_Xanadu](https://en.wikipedia.org/wiki/Project_Xanadu)

#### Bootstrap

* [https://getbootstrap.com/components/](https://getbootstrap.com/components/)
* [https://getbootstrap.com/css/](https://getbootstrap.com/css/)
* [https://getbootstrap.com/javascript/](https://getbootstrap.com/javascript/)

#### Responsiveness

* [http://www.quirksmode.org/mobile/](http://www.quirksmode.org/mobile/)
* [https://developers.google.com/web/fundamentals/design-and-ui/responsive/fundamentals/?hl=en](https://developers.google.com/web/fundamentals/design-and-ui/responsive/fundamentals/?hl=en)
* [http://learn.shayhowe.com/advanced-html-css/responsive-web-design/](http://learn.shayhowe.com/advanced-html-css/responsive-web-design/)
* [https://bradfrost.github.io/this-is-responsive/patterns.html](https://bradfrost.github.io/this-is-responsive/patterns.html)

#### Drag and drop

* [http://www.html5rocks.com/en/tutorials/dnd/basics/](http://www.html5rocks.com/en/tutorials/dnd/basics/)
* [http://www.html5rocks.com/en/tutorials/file/dndfiles/](http://www.html5rocks.com/en/tutorials/file/dndfiles/)
* [http://www.quirksmode.org/js/dragdrop.html](http://www.quirksmode.org/js/dragdrop.html)
* [http://www.quirksmode.org/js/events_properties.html#position](http://www.quirksmode.org/js/events_properties.html#position)
* [http://www.petercollingridge.co.uk/interactive-svg-components/draggable-svg-element](http://www.petercollingridge.co.uk/interactive-svg-components/draggable-svg-element)

#### Overlays

* [http://tympanus.net/codrops/2013/11/07/css-overlay-techniques/](http://tympanus.net/codrops/2013/11/07/css-overlay-techniques/)
* [https://stackoverflow.com/questions/1401658/html-overlay-which-allows-clicks-to-fall-through-to-elements-behind-it](https://stackoverflow.com/questions/1401658/html-overlay-which-allows-clicks-to-fall-through-to-elements-behind-it)

#### Layout

* [http://adamschwartz.co/magic-of-css/chapters/2-layout/](http://adamschwartz.co/magic-of-css/chapters/2-layout/)
* [http://adamschwartz.co/magic-of-css/chapters/3-tables/](http://adamschwartz.co/magic-of-css/chapters/3-tables/)
* [https://css-tricks.com/snippets/css/a-guide-to-flexbox/](https://css-tricks.com/snippets/css/a-guide-to-flexbox/)
* [https://drafts.csswg.org/css-align/](https://drafts.csswg.org/css-align/)

#### Transitions

* [http://adamschwartz.co/magic-of-css/chapters/6-transitions/](http://adamschwartz.co/magic-of-css/chapters/6-transitions/)

#### Fullscreen

* [https://fullscreen.spec.whatwg.org/](https://fullscreen.spec.whatwg.org/)
* [https://stackoverflow.com/questions/1125084/how-to-make-in-javascript-full-screen-windows-stretching-all-over-the-screen](https://stackoverflow.com/questions/1125084/how-to-make-in-javascript-full-screen-windows-stretching-all-over-the-screen)

#### CSS quirks

* [http://designshack.net/articles/css/everything-you-never-knew-about-css-floats/](http://designshack.net/articles/css/everything-you-never-knew-about-css-floats/)
* [http://colintoh.com/blog/display-table-anti-hero](http://colintoh.com/blog/display-table-anti-hero)
* [https://developer.mozilla.org/en-US/docs/Web/Guide/CSS/Media_queries](https://developer.mozilla.org/en-US/docs/Web/Guide/CSS/Media_queries)
* [http://www.csszengarden.com/](http://www.csszengarden.com/)
* [http://getbootstrap.com/](http://getbootstrap.com/)
* [https://www.amazon.ca/The-Non-Designers-Design-Book-Edition/dp/0321534042](https://www.amazon.ca/The-Non-Designers-Design-Book-Edition/dp/0321534042)

#### Accessibility

* [https://w3c.github.io/aria-in-html/](https://w3c.github.io/aria-in-html/)
* [https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA](https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA)
* [https://developer.mozilla.org/en-US/docs/Web/Accessibility/Keyboard-navigable_JavaScript_widgets](https://developer.mozilla.org/en-US/docs/Web/Accessibility/Keyboard-navigable_JavaScript_widgets)

### JavaScript

* [JavaScript Garden](https://bonsaiden.github.io/JavaScript-Garden/)
* [https://dorey.github.io/JavaScript-Equality-Table/](https://dorey.github.io/JavaScript-Equality-Table/)
* [https://kangax.github.io/compat-table/es6/](https://kangax.github.io/compat-table/es6/)
* [Promise PolyFill](https://www.promisejs.org/)
* [Tail-call optimization without trampoline](http://glat.info/jscheck/tomrec.html)
* [JavaScript has a Unicode problem](https://mathiasbynens.be/notes/javascript-unicode)

#### Event handling

* requestAnimationFrame
* [event reference](https://developer.mozilla.org/en-US/docs/Web/Events)
* [http://www.quirksmode.org/js/contents.html#events](http://www.quirksmode.org/js/contents.html#events)
* [http://eloquentjavascript.net/14_event.html](http://eloquentjavascript.net/14_event.html)
* [A crash course in how DOM events work](http://blog.bitovi.com/a-crash-course-in-how-dom-events-work/)
* [JavaScript Madness: Keyboard Events](http://unixpapa.com/js/key.html)

#### Web workers

* [https://developer.mozilla.org/en-US/docs/Web/API/Web_Workers_API/Using_web_workers](https://developer.mozilla.org/en-US/docs/Web/API/Web_Workers_API/Using_web_workers)
* [https://stackoverflow.com/questions/5408406/web-workers-without-a-separate-javascript-file](https://stackoverflow.com/questions/5408406/web-workers-without-a-separate-javascript-file)

#### Content editable

* [https://developer.mozilla.org/en-US/docs/Web/Guide/HTML/Content_Editable](https://developer.mozilla.org/en-US/docs/Web/Guide/HTML/Content_Editable)
* [https://stackoverflow.com/questions/16050762/jquery-contenteditable-new-paragraph-on-keypress-works-on-original-but-not-g](https://stackoverflow.com/questions/16050762/jquery-contenteditable-new-paragraph-on-keypress-works-on-original-but-not-g)
* [http://www.quirksmode.org/js/keys.html](http://www.quirksmode.org/js/keys.html)
* [http://www.cambiaresearch.com/articles/15/javascript-key-codes](http://www.cambiaresearch.com/articles/15/javascript-key-codes)
* [Why ContentEditable is Terrible](https://medium.engineering/why-contenteditable-is-terrible-122d8a40e480)
* [Faking an editable control in browser JavaScript](http://marijnhaverbeke.nl/blog/browser-input-reading.html)

#### Security

* [OWASP XSS Prevention Rules](https://www.owasp.org/index.php/XSS_(Cross_Site_Scripting)_Prevention_Cheat_Sheet#RULE_.233_-_JavaScript_Escape_Before_Inserting_Untrusted_Data_into_HTML_JavaScript_Data_Values)
* [Misuse of DOM text methods](http://benv.ca/2012/10/02/you-are-probably-misusing-DOM-text-methods/)
* [Automatic charset detection issue](http://wonko.com/post/html-escaping)
* [Mustache escaping](https://github.com/janl/mustache.js/blob/cd06b22dabdaeffe3e4c74ee02bd492a11bbb740/mustache.js#L71)
* [What to escape in a script element](https://stackoverflow.com/questions/6908866/what-exactly-do-i-have-to-escape-inside-a-script-element)

### Misc

* [http://apress.jensimmons.com/v5/pro-html5-programming/ch0.html](http://apress.jensimmons.com/v5/pro-html5-programming/ch0.html)
* [http://diveintohtml5.info/table-of-contents.html](http://diveintohtml5.info/table-of-contents.html)
* [https://www.kirupa.com/learn/index.htm](https://www.kirupa.com/learn/index.htm)

[https://stackoverflow.com/questions/4112289/what-is-the-html-tabindex-attribute](https://stackoverflow.com/questions/4112289/what-is-the-html-tabindex-attribute)

#### HTML attributes

* [elements to attributes](https://simon.html5.org/html-elements)
* [attributes to elements](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes)
* [standard custom data attributes](https://stackoverflow.com/a/18227550)
* [efficient custom data attributes](http://calendar.perfplanet.com/2012/efficient-html5-data-attributes/)

#### CSS Properties

* [CSS 2.1](https://www.w3.org/TR/CSS21/propidx.html)
* [CSS 3](http://www.quackit.com/css/css3/properties/)
* [https://developer.mozilla.org/en-US/docs/Web/CSS/Reference](https://developer.mozilla.org/en-US/docs/Web/CSS/Reference)

## UI ideas

### Input

* device agnosticism
    * onactivate/onfocus/oninput/onchange vs. onclick/onmouseX/onkeypress
    * accessibility: optional keyboard versions of mouse uses
* explicit event interception: stopPropagation the default

### Tools

* context-sensitive, categorized input command menu
* dictionary: searchable list of reference-able names and their meanings
    * includes palette for primitive and user-defined terms and embeddable controls
* bookmarks: anchor points in the workspace that can be quickly jumped to
    * also summarizes embedded links for convenience
* workspace: initially a single sheet; sheets may be embedded in other sheets
* explanation: an automatically-built sheet describing a context's provenance

### Term visualization

* source vs. computed
    * computed adjacent to computing formulas
    * boolean
        * source: checkbox
        * derivative?
    * string
        * source: textarea
        * derivative: label
    * number, color, date/time: corresponding input
    * spatial vector, graphic: canvas
* alternative formulations selectable via radio button
* aggregates/compounds: framed/bordered panels
    * unordered: separated terms
    * ordered: tightly packed terms, optionally numbered
    * associated: terms have labels
* context holes point to dependencies
* derivatives point to originals
* conflicts and their traces: highlighted

### Layout

* default to wrapping, horizontal layout
* allow forced vertical breaks
* panels/sheets to group terms and descriptions without affecting namespaces
    * panel chooses horizontal or vertical layout for its elements
* stacked sheets for spatial compression
    * tabbed or [multi-]accordion selection
* layers
* spacers

### Actions

* halo menus
    * grab/move for two-click (start and end) drag and drop
    * rotate
    * resize
    * style
    * delete
    * derive/copy
* sheet-level
    * introduce paragraphs of descriptive text
    * introduce links
    * new term
    * label term
    * edit term formula/source
        * include alternative formula (for quick toggling when testing multiple ideas)
    * wrap/group terms as compound
    * dissolve compound elements into its surroundings
    * toggle compound layout orientation; horizontal, vertical
    * reorder terms within compound
    * move term into compound
    * extract term from compound
    * retract/extend compound summaries
    * derive new term from an existing one
    * detach derivative from original to form an independent copy
    * generate a series of derivations along the length of an ordered compound
        * similar to a combination of zip and map
    * prepend an independent term to the head of a derivation series
        * supports accumulator-passing fold/scan computations
    * set, list, and relational operations
        * union, difference, filter/select, project, join
* formula-level
    * reference by label/name
        * dropdown selection to resolve name ambiguity
    * reference by pointing
    * embed literal atom
    * introduce function application
    * introduce case analysis or conditional; designate branch results (see: functions)
    * expand/inline/descend-into function application; w/ corresponding ascend
    * introduce function
        * parameterize a group of terms over a subset of its sources
        * designate a [group of] term[s] as the externally-visible output
    * introduce algebraic data constructors

### Test cases

* [7GUIs](https://github.com/eugenkiss/7guis/wiki)
* spreadsheets
* databases
* simulations
* grammar of graphics
* input-dependent animation (time and space)
* machine learning algorithms
* interactive goban

## TODO

* strict mode
* mouse/keyboard position selection
* editable content
* progressive enhancement
* file upload/re-seating
* geolocation
* end-to-end encryption
* efficient DOM update
