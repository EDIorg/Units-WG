<?php
# Web service to identify matching QUDT units based on a Lookup Table
#  CC0, John Porter, 2023
$DEBUG=0;

if ($_REQUEST['rawunit']=="") die("Syntax for this web service is:<br>\n http://xxxxxWebServiceURL?rawunit=meter&returntype=annotation        <br>\n
where meter is the unit to search for. Possible returntype (output options) are: <br>\n
annotation - annotation as text (may look odd on web browser unless you use view source)     <br>\n
annotationhttp - annotation as html for viewing on a web browser      <br>\n
uri - return QUDT URI                    <br>\n
unit - return QUDT Unit                  <br>\n
label - return QUDT label                  <br>\n
full - return QUDT Unit, QUDT Label, QUDT URI, QUDT Dimension Vector, Conversion multiplier, and description (as text and as LaTex) if available (NA otherwise)  <br>\n
fullhttp - same but with formatting codes for html included       <br>\n
");


if($DEBUG)print("Debug Active\n");

if($DEBUG)print_r($_REQUEST);

$row = 1;
if (($handle = fopen("https://raw.githubusercontent.com/EDIorg/Units-WG/main/RCode_JP/DataFiles4R/unitsWithQUDTInfo.csv", "r")) !== FALSE) {
    while (($data = fgetcsv($handle, 1000, ",","\"")) !== FALSE) {
        if ($DEBUG) print("<br>\n row: ".$row." raw unit: ".$_REQUEST['rawunit'].", QUDT unit: ".$data[0]."<br>\n");
    	if  (strcasecmp($_REQUEST['rawunit'],$data[0])==0){
	    if (strcasecmp($_REQUEST['returntype'],'uri') == 0){
	       print($data[3]);
	    }elseif (strcasecmp($_REQUEST['returntype'],'full') == 0){
	               for ($c=1; $c < 8; $c++) {
		       	   echo $data[$c]."\n";
		       }
	    }elseif (strcasecmp($_REQUEST['returntype'],'fullhttp') == 0){
	               for ($c=1; $c < 8; $c++) {
		       	   echo $data[$c]."<br>\n";
		       }
	    }elseif (strcasecmp($_REQUEST['returntype'],'json') == 0){
	    	    echo("{ ");
		           echo("\"unit\":\"".$data[0]."\", \n");
		           echo("\"qudtUnit\":\"".$data[1]."\", \n");
		           echo("\"qudtLabel\":\"".$data[2]."\", \n");
		           echo("\"qudtURI\":\"".$data[3]."\", \n");
			   echo("\"qudtDimension\":\"".$data[4]."\", \n");
			   echo("\"qudtMultiplier\":\"".$data[5]."\", \n");
			   echo("\"qudtDescription\":\"".$data[6]."\" \n");
			   echo("}\n");
	    }elseif (strcasecmp($_REQUEST['returntype'],'label') == 0){
	       print($data[2]);
	    }elseif (strcasecmp($_REQUEST['returntype'],'annotationhttp') == 0){
	           print('&lt;annotation&gt;<br>
  &lt;propertyURI label="has unit"&gt;http://qudt.org/schema/qudt/hasUnit&lt;/propertyURI&gt;<br>
  &lt;valueURI label="'.$data[2].'"&gt;'.$data[3]."&lt;/valueURI&gt;<br>
&lt;/annotation&gt;");
	    }elseif (strcasecmp($_REQUEST['returntype'],'annotation') == 0){
	           print('<annotation>
  <propertyURI label="has unit">http://qudt.org/schema/qudt/hasUnit</propertyURI>
  <valueURI label="'.$data[2].'">'.$data[3]."</valueURI>
</annotation>");
	    }else{
    	       print($data[1]);
	    }
if ($DEBUG){
   $num = count($data);
        for ($c=0; $c < $num; $c++) {
            echo $row." ".$c." ".$data[$c] . "<br />\n";
        }
}
# since we found a match, quit
die();
    } # end if match
    $row++;
    } # end while
    print("No_Match");
    fclose($handle);

}else die("couldn't open units file\n");


?>
