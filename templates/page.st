<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
  <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
  <meta name="google-site-verification" content="cVvYc9qXOvJLk8img75XRz7R0mk1pDwMsHOYLHCvSrw" />
  $if(feed)$
  <link href="$base$/_feed/" type="application/atom+xml" rel="alternate" title="Sitewide ATOM Feed" />
  <link href="$base$/_feed$pageUrl$" type="application/atom+xml" rel="alternate" title="This page's ATOM Feed" />
  $endif$
  <title>$wikititle$ - $pagetitle$</title>
  $if(printable)$
  <link href="$base$/css/print.css" rel="stylesheet" media="all" type= "text/css" />
  $else$
  <link href="$base$/css/custom.css" rel="stylesheet" media="screen, projection" type="text/css" />
  <link href="$base$/css/print.css" rel="stylesheet" media="print" type= "text/css" />
  $endif$
  <!--[if IE]><link href="$base$/css/ie.css" rel="stylesheet" media="screen, projection" type="text/css" /><![endif]-->
  <link rel="icon" type="image/x-icon" href="$base$/img/favicon.png" />
</head>
<body>
  <div id="all">
    <div id="maincol" class="main-content">
      $content()$
    </div>
    <div id="tools">
      $if(sitenav)$
      $sitenav()$
      $endif$
      $if(pagetools)$
      $pagetools()$
      $endif$
      $if(markuphelp)$
      $markuphelp()$
      $endif$
      <div class="filler"></div>
    </div>
    $footer()$
  </div>
  $javascripts$
  $expire()$
  $getuser()$
  <script type="text/javascript">
    var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");
    document.write("\<script src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'>\<\/script>" );
  </script>
  <script type="text/javascript">
    var pageTracker = _gat._getTracker("UA-866665-1");
    pageTracker._initData();
    pageTracker._trackPageview();
  </script>
</body>
</html>
