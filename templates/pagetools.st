<div class="tool-category">
  <fieldset>
    <legend>This page</legend>
    <ul>
      <li><a href="$base$$pageUrl$">Latest version</a></li>
      <li><a href="$base$/_edit$pageUrl$">Edit</a></li>
      <li><a href="$base$/_history$pageUrl$">History</a></li>
      <li><a href="$base$/_showraw$pageUrl$$if(revision)$?revision=$revision$$endif$">Raw page source</a></li>
      <li><a href="$base$$pageUrl$?printable$if(revision)$&amp;revision=$revision$$endif$">Printable version</a></li>
      <li><a href="$base$/_delete$pageUrl$">Delete</a></li>
      $if(feed)$
      <li><a href="$base$/_feed$pageUrl$" type="application/atom+xml" rel="alternate" title="This page's ATOM Feed">Atom feed</a></li>
      $endif$
      <li>$exportbox$</li>
    </ul>
  </fieldset>
</div>
