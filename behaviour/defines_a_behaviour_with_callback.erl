-module (defines_a_behaviour_with_callback).

-callback init( atom()) -> ok.
-callback handle(atom() ) -> ok.
 


