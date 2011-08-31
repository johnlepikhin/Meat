
let main ~js content = <<
<html>
	<head>
		<title>Просто со вкусом</title>
		<script type="text/javascript" src=$js$/>
		<link rel="stylesheet" type="text/css" href="/css/css_main.css"/>
	</head>
	<body id=$Common.body_id$>
		$content$
		<div class=$Css_main.Main.Footer.div$>
			© $str:Config.start_year$–$str:Config.current_year$ $str:Config.copyright$
		</div>
	</body>
</html>
>>
