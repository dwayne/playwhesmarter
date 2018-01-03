module Helpers
  module Time
    def now
      ::Time.now.getlocal('-04:00')
    end
  end
end
