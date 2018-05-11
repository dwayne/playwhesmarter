module Helpers
  module Navigation
    PATHS = [
      '/',
      '/results/',
      '/hiding-marks/',
      '/mark-frequencies-by-year-month/'
    ]

    def active(name)
      path = @item.path
      is_active =
        case name
        when 'home'
          path == PATHS[0]
        when 'results'
          path == PATHS[1]
        when 'hiding-marks'
          path == PATHS[2]
        when 'mark-frequencies'
          path == PATHS[3]
        when 'tools'
          path == PATHS[1] || path == PATHS[2] || path == PATHS[3]
        else
          false
        end

      is_active ? 'active' : ''
    end
  end
end
