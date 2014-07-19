  <script src="/js/viz.js"></script>
  <script>
    function unHtmlify(s) {
      return s.replace(/&lt;/g, "<").replace(/&gt;/g, ">");
    }
    
    var graphPres = document.getElementsByClassName("dot");
    for (var i = 0; i < graphPres.length; ++i) {
      var graphPre = graphPres[i];
      var graphCode = unHtmlify(graphPre.getElementsByTagName('code')[0].innerHTML);
      var graphSvg = document.createElement("div");
      graphSvg.className = "diagram";
      try {
        graphSvg.innerHTML = Viz(graphCode, "svg");
        graphPre.parentNode.replaceChild(graphSvg, graphPre);
      } catch(e) {
        console.log(e.toString());
      }
    }
  </script>  
