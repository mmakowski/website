<div class="tool-category">
  <fieldset>
    <legend>Site</legend>
    <ul>
      <li><a href="$base$/">Home page</a></li>
      <li><a href="$base$/_index">All pages</a></li>
      <li><a href="$base$/_categories">Categories</a></li>
      <li><a href="$base$/_random">Random page</a></li>
      <li><a href="$base$/_activity">Recent activity</a></li>
      $if(wikiupload)$
      <li><a href="$base$/_upload">Upload a file</a></li>
      $endif$
      $if(feed)$
      <li><a href="$base$/_feed/" type="application/atom+xml" rel="alternate" title="ATOM Feed">Atom feed</a> <img alt="feed icon" src="$base$/img/icons/feed.png"/></li>
      $endif$
    </ul>
  </fieldset>
</div>
