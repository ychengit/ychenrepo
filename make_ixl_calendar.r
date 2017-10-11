make_ixl_calendar <- function() {
    x_vec_subject_name = c("Geometry", "Langugage 10th Grade", "Science 9th Grade", "Social 9th Grade", "Spanish");
    x_vec_geometry = c(10, 8, 7, 6, 12);
    x_vec_language = c(10, 3, 5);
    x_vec_science = c(10, 2, 8);
    x_vec_social = c(10, 5, 7);
    x_vec_spanish = c(2, 3);
    x_list_subject_section = list(x_vec_geometry, x_vec_language, x_vec_science, x_vec_social, x_vec_spanish);

    
    xa = 0;
    for (x_vec_a in x_list_subject_section) {
        xa = xa + 1;
        xb = 0;
        xc = 0;
        x_date_start = as.Date("2017-10-10");

        for (xd in x_vec_a) {
            xb = xb + 1;
            x_char = "A";
            if (xb <= 26) {
                x_char = rawToChar(as.raw(as.numeric(charToRaw("A"))+xb-1));
            } else if (xb <= 52) {
                x_char = rawToChar(as.raw(as.numeric(charToRaw("AA"))+xb-26-1));
            }

            for (xe in 1:xd) {
                xc = xc + 1;
                x_date_a = x_date_start + xc;
                x_line = paste(x_vec_subject_name[xa], " ", x_char, xe, ", ", x_date_a, ", ", x_date_a, sep="");
                print(x_line);
            }
        }
    }
}

make_ixl_calendar();
