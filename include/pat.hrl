-record(email, {sender           :: pat:address(),
                from             :: pat:address(),
                recipients       :: [pat:address()],
                subject = <<"">> :: binary(),
                message          :: binary(),
                headers = []     :: [{binary(), binary()}]}).
