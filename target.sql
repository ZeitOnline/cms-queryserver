-- This is a sample query similar to the ones we want to generate

SELECT DISTINCT 
       tmp_1.uri,                                       -- This is the p-list (short for projection-list)
       tmp_2.VALUE AS "{http://foo/founz}/ressort",     -- this list will be created from all binders
       tmp_3.VALUE AS "{http://foo/founz}/author"       --
FROM

  -- A filter/predicate
     (SELECT uri, namespace, name 
        FROM facts 
         WHERE namespace = 'http://namespaces.zeit.de/CMS/document' 
         AND   name = 'year'  
         AND value = '2006') AS tmp_1  
INNER JOIN
     (SELECT uri, namespace, name 
        FROM facts 
        WHERE namespace = 'http://namespaces.zeit.de/CMS/document' 
        AND   name = 'volume'  AND value = '32') AS tmp_0  ON (tmp_1.URI=tmp_0.URI)  
INNER JOIN
     (SELECT uri, namespace, name
        FROM facts
        WHERE namespace = 'http://namespaces.zeit.de/CMS/workflow'
        AND   name = 'status'  AND value = 'OK') AS tmp_4  ON (tmp_4.URI=tmp_0.URI)
INNER JOIN
  -- A binder 
      (SELECT uri, namespace, name, value 
         FROM facts 
         WHERE namespace = 'http://namespaces.zeit.de/CMS/document'
         AND   name = 'ressort') AS tmp_2  ON (tmp_0.URI=tmp_2.URI) 
INNER JOIN
  -- another binder     
      (SELECT uri, namespace, name, value 
         FROM facts 
         WHERE namespace = 'http://namespaces.zeit.de/CMS/document'
         AND   name = 'author') AS tmp_3  ON (tmp_2.URI=tmp_3.URI) 

ORDER BY tmp_1.uri;
