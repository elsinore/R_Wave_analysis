window.MathJax = {
  jax: ["input/TeX","output/CommonHTML"],
  extensions: ["tex2jax.js", "asciimath2jax.js", "mml2jax.js", "MathMenu.js", "MathZoom.js"],
  TeX: {
    extensions: ["AMSmath.js", "AMSsymbols.js", "autoload-all.js"]
  },
  tex2jax: {
    inlineMath: [
      ['$', '$'],
      ["\\(", "\\)"]
    ],
    processEscapes: true
  },
  showProcessingMessages: false,
  messageStyle: "none",
  menuSettings: { zoom: "Click" },
  AuthorInit: function() {
    MathJax.Hub.Register.StartupHook("End", function() {
            var timeout = false, // holder for timeout id
            delay = 250; // delay after event is "complete" to run callback
            var shrinkMath = function() {
              var dispFormulas = document.getElementsByClassName("formula");
              if (dispFormulas){
                // caculate relative size of indentation
                var contentTest = document.getElementsByTagName("body")[0];
                var nodesWidth = contentTest.offsetWidth;
                // if you have indentation
                var mathIndent = MathJax.Hub.config.displayIndent; //assuming px's
                var mathIndentValue = mathIndent.substring(0,mathIndent.length - 2);
                for (var i=0; i<dispFormulas.length; i++){
                  var dispFormula = dispFormulas[i];
                  var wrapper = dispFormula.getElementsByClassName("MathJax_Preview")[0].nextSibling;
                  var child = wrapper.firstChild;
                  wrapper.style.transformOrigin = "center"; //or top-left if you left-align your equations
                  var oldScale = child.style.transform;
                  var newValue = Math.min(0.80*dispFormula.offsetWidth / child.offsetWidth,1.0).toFixed(2);
                  var newScale = "scale(" + newValue + ")";
                  if(!(newScale === oldScale)){
                    wrapper.style.transform = newScale;
                    wrapper.style["margin-left"]= Math.pow(newValue,4)*mathIndentValue + "px";
                    var wrapperStyle = window.getComputedStyle(wrapper);
                    var wrapperHeight = parseFloat(wrapperStyle.height);
                    wrapper.style.height = "" + (wrapperHeight * newValue) + "px";
                    if(newValue === "1.00"){
                      wrapper.style.cursor = "";
                      wrapper.style.height = "";
                    }
                    else {
                      wrapper.style.cursor = "zoom-in";
                    }
                  }

                }
            }
            };
            shrinkMath();
            window.addEventListener('resize', function() {
              clearTimeout(timeout);
              timeout = setTimeout(shrinkMath, delay);
            });
          });
  }
};
(function(d, script) {
  script = d.createElement('script');
  script.type = 'text/javascript';
  script.async = true;
  script.onload = function() {
    // remote script has loaded
  };
  script.src = './MathJax/MathJax.js';
  d.getElementsByTagName('head')[0].appendChild(script);
}(document));
